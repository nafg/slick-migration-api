package slick
package migration.api

import java.sql.Types

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.control.NonFatal

import slick.jdbc.JdbcProfile
import slick.jdbc.meta._
import slick.lifted.AbstractTable

import com.typesafe.slick.testkit.util.JdbcTestDB
import org.scalactic.TypeCheckedTripleEquals
import org.scalactic.source.Position
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{BeforeAndAfterAll, FunSuite, Inside}


abstract class DbTest[P <: JdbcProfile](val tdb: JdbcTestDB {val profile: P})
                                       (implicit protected val dialect: Dialect[P])
  extends FunSuite with Inside with BeforeAndAfterAll with TypeCheckedTripleEquals {

  lazy val database = tdb.createDB
  lazy val session = database.createSession()
  lazy val sessionDB = tdb.createSingleSessionDatabase(session)

  lazy val profile: P = tdb.profile

  import profile.api._

  override def beforeAll(): Unit = tdb.cleanUpBefore()
  override def afterAll(): Unit = {
    sessionDB.close()
    session.close()
    database.close()
    tdb.cleanUpAfter()
  }

  val catalog, schema = Option("")

  val noActionReturns: ForeignKeyAction = ForeignKeyAction.NoAction

  def runAction[A](action: DBIO[A])(implicit pos: Position): A = {
    try Await.result(sessionDB.run(action), Duration.Inf)
    catch {
      case e: TestFailedException => throw e
      case NonFatal(e)            => fail(e.getMessage, e)
    }
  }

  def getTables: DBIO[Vector[MTable]] = MTable.getTables(catalog, schema, None, Some(Seq("TABLE")))

  def getTable(name: String)(implicit pos: Position): DBIO[MTable] =
    getTables.map { tables =>
      tables
        .find(_.name.name == name)
        .getOrElse {
          withClue(s"tables: $tables") {
            fail(s"No table named $name")
          }
        }
    }

  def getTable[E <: AbstractTable[_]](table: TableQuery[E])(implicit pos: Position): DBIO[MTable] =
    getTable(table.baseTableRow.tableName)

  val longJdbcType = Types.BIGINT

  /**
   * How JDBC metadata returns a column's default string value
   */
  def columnDefaultFormat(s: String) = s"'$s'"

  def runMigration(migration: Migration): Unit = runAction(migration())

  def withBeforeAndAfter[A](migration: Migration)(action: => DBIO[A])(f: (A, A) => Unit)(implicit pos: Position): Unit =
    runAction {
      for {
        before <- action
        _ <- migration()
        after <- action
      } yield f(before, after)
    }

  def getColumns[E <: AbstractTable[_]](table: TableQuery[E]): DBIO[Vector[MColumn]] =
    getTable(table).flatMap(_.getColumns)


  class TestTable(tag: Tag) extends Table[Long](tag, "test_table") {
    def * = id

    val id = column[Long]("id", O.AutoInc, O.PrimaryKey)

    val id1 = column[Int]("id_1")
    val id2 = column[String]("id_2", O.Length(254))

    val strWithDefault = column[String]("str_with_default", O.Default("abc"))

    val int1Nullable = column[Option[Int]]("int1")

    val int1 = column[Int]("int1")
    val int2 = column[Int]("int2")
    val int3 = column[Int]("int3")

    def other = column[Long]("other")

    val index1 = index("index1", int1)
    val index2 = index("index2", (int2, int3), unique = true)

    val compoundPK = primaryKey("PRIMARY", (id1, id2)) // note mysql will always use the name "PRIMARY" anyway

    lazy val fk = foreignKey("fk_other", other, TestTable)(_.id, ForeignKeyAction.NoAction, ForeignKeyAction.Cascade)
  }

  lazy val TestTable = TableQuery[TestTable]

  test("create, drop") {
    val tm = TableMigration(TestTable)
    val createTable = tm.create.addColumns(_.id, _.strWithDefault)

    withBeforeAndAfter(createTable)(getTables) { (before, after) =>
      try {
        val tableName = TestTable.baseTableRow.tableName
        inside(after filterNot before.contains) {
          case (table @ MTable(MQName(_, _, `tableName`), "TABLE", _, _, _, _)) +: xs if xs.isEmpty =>
            val cols =
              runAction(table.getColumns)
                .map(c => c.name -> c)
                .toMap
            assert(cols.keySet === Set("id", "str_with_default"))
            val default = Some(columnDefaultFormat("abc"))
            inside(cols("id")) {
              case MColumn(_, _, `longJdbcType`, _, _, _, _, Some(false), _, _, _, _, _, _, _, Some(true)) =>
            }
            inside(cols("str_with_default")) {
              case MColumn(_, _, Types.VARCHAR, _, _, _, _, Some(false), _, `default`, _, _, _, _, _, Some(false)) =>
            }
        }

        assert(tm.create.reverse === tm.drop)

      } finally
        runMigration(tm.drop)

      assert(runAction(getTables) === before)
    }
  }

  test("addColumns") {
    val tm = TableMigration(TestTable)
    runMigration(tm.create.addColumns(_.int2))

    try {
      val addOtherColumns = tm.addColumns(_.int1Nullable, _.strWithDefault)
      assert(addOtherColumns.reverse == tm.dropColumns(_.strWithDefault, _.int1Nullable))

      withBeforeAndAfter(addOtherColumns)(getColumns(TestTable)) { (before, after) =>
        assert(before.length === 1)
        assert(after.length === 3)
      }

    } finally
      runMigration(tm.drop)
  }

  test("addIndexes, dropIndexes") {

    def indexes =
      getTable(TestTable).flatMap(_.getIndexInfo())
        .map(_.groupBy(i => (i.indexName, !i.nonUnique)).transform((_, is) => is.sortBy(_.ordinalPosition).map(_.column)))

    val tm = TableMigration(TestTable)
    val createIndexes = tm.addIndexes(_.index1, _.index2)

    runMigration(tm.create.addColumns(_.id, _.int1, _.int2, _.int3))

    try
      withBeforeAndAfter(createIndexes)(indexes) { (_, i2) =>
        assert(i2(Some("index1") -> false) === Vector(Some("int1")))
        assert(i2(Some("index2") -> true) === Vector(Some("int2"), Some("int3")))

        assert(createIndexes.reverse == tm.dropIndexes(_.index2, _.index1))

        withBeforeAndAfter(createIndexes.reverse)(indexes) { (_, i4) =>
          assert(i4.keys.flatMap(_._1).exists(Set("index1", "index2")) === false)
        }
      }
    finally
      runMigration(tm.drop)
  }

  test("rename") {
    val tm = TableMigration(TestTable)

    def tableNames = getTables.map(_.map(_.name.name).toSet)

    runMigration(tm.create.addColumns(_.int1))

    val renameMigration = tm.rename("other_name")

    try
      withBeforeAndAfter(renameMigration)(tableNames) { (t2, t3) =>
        assert(t3 -- t2 === Set("other_name"))
        assert(t2 -- t3 contains TestTable.baseTableRow.tableName)
      }
    finally
      runMigration(renameMigration.reverse & tm.drop)
  }

  test("renameIndex") {
    val tm = TableMigration(TestTable)

    def indexNames =
      getTables.flatMap(ts => DBIO.sequence(ts.map(_.getIndexInfo())))
        .map(_.flatMap(_.flatMap(_.indexName)).toSet)

    withBeforeAndAfter(tm.create.addColumns(_.int1).addIndexes(_.index1))(indexNames) { case (_, i2) =>
      assert(i2 === Set("index1"))

      try
        withBeforeAndAfter(tm.renameIndex(_.index1, "other_index_name"))(indexNames) { (i3, i4) =>
          assert(i4 -- i3 === Set("other_index_name"))
        }
      finally
        runMigration(tm.drop)
    }
  }
}

trait CompleteDbTest { this: DbTest[_ <: JdbcProfile] =>
  import profile.api._

  test("addPrimaryKeys, dropPrimaryKeys") {
    def pks =
      getTable(TestTable).flatMap(_.getPrimaryKeys)
        .map(_.groupBy(_.pkName).transform((_, ks) => ks.sortBy(_.keySeq).map(_.column)))

    val tm = TableMigration(TestTable)

    runMigration(tm.create.addColumns(_.id1, _.id2))

    try
      withBeforeAndAfter(tm.addPrimaryKeys(_.compoundPK))(pks) { (pks1, pks2) =>
        assert(!pks1.contains(Some("PRIMARY")))
        assert(pks2(Some("PRIMARY")) === Vector("id_1", "id_2"))
        assert(tm.addPrimaryKeys(_.compoundPK).reverse == tm.dropPrimaryKeys(_.compoundPK))

        withBeforeAndAfter(tm.dropPrimaryKeys(_.compoundPK))(pks) { (_, pks3) =>
          assert(pks3 === pks1)
        }
      }
    finally
      runMigration(tm.drop)
  }

  test("addForeignKeys, dropForeignKeys") {
    def tableFks =
      getTables
        .flatMap(ts => DBIO.sequence(ts.map(t => t.getExportedKeys.map(t.name.name -> _))))
        .map(_.toMap)

    val tm = TableMigration(TestTable)
    val createForeignKey = tm.addForeignKeys(_.fk)
    val dropForeignKey = createForeignKey.reverse

    runMigration(tm.create.addColumns(_.id, _.other))

    try
      withBeforeAndAfter(createForeignKey)(tableFks) { (fks2, fks3) =>
        inside(fks3(TestTable.baseTableRow.tableName)) {
          case Vector(MForeignKey(_, "id", MQName(_, _, "test_table"), "other", _, `noActionReturns`, ForeignKeyAction.Cascade, Some("fk_other"), _, _)) =>
        }

        assertResult(TestTable.baseTableRow.fk.fks.toList) {
          dropForeignKey.actions.collect { case TableMigration.Action.DropForeignKey(fk) => fk }
        }

        assert(dropForeignKey == tm.dropForeignKeys(_.fk))

        withBeforeAndAfter(dropForeignKey)(tableFks) { (_, fks4) =>
          assert(fks4 === fks2)
        }
      }
    finally
      runMigration(tm.drop)
  }

  test("dropColumns") {
    val tm = TableMigration(TestTable)
    runMigration(tm.create.addColumns(_.id, _.int1))

    try
      withBeforeAndAfter(tm.dropColumns(_.int1))(getColumns(TestTable)) { (before, after) =>
        assert(before.length === 2)
        assert(after.length === 1)
      }
    finally
      runMigration(tm.drop)

    assert(tm.addColumns(_.id, _.int1).reverse == tm.dropColumns(_.int1, _.id))
  }

  test("alterColumnTypes") {
    val tm = TableMigration(TestTable)
    runMigration(tm.create.addColumns(_.column[java.sql.Date]("str_with_default")))

    try
      withBeforeAndAfter(tm.alterColumnTypes(_.strWithDefault))(getColumns(TestTable)) { (before, after) =>
        assert(before.map(_.sqlType) === Vector(Types.DATE))
        assert(after.map(_.sqlType) === Vector(Types.VARCHAR))
      }
    finally
      runMigration(tm.drop)
  }

  test("alterColumnDefaults") {
    val tm = TableMigration(TestTable)
    runMigration(tm.create.addColumns(_.column[String]("str_with_default", profile.columnOptions.Length(254))))

    try
      withBeforeAndAfter(tm.alterColumnDefaults(_.strWithDefault))(getColumns(TestTable)) { (before, after) =>
        assert(before.map(_.columnDef) === Vector(None))
        assert(after.map(_.columnDef) === Vector(Some(columnDefaultFormat("abc"))))
    } finally
      runMigration(tm.drop)
  }

  test("alterColumnNulls") {
    val tm = TableMigration(TestTable)

    runMigration(tm.create.addColumns(_.int1Nullable))

    try
      withBeforeAndAfter(tm.alterColumnNulls(_.int1))(
        DBIO.seq(TestTable.map(_.int1Nullable) += None, TestTable.delete).asTry
      ) { (before, after) =>
        assert(before.isSuccess)
        assert(after.isFailure)
      }
    finally
      runMigration(tm.drop)
  }

  test("renameColumn") {
    val tm = TableMigration(TestTable)
    runMigration(tm.create.addColumns(_.int1))

    try
      withBeforeAndAfter(tm.renameColumn(_.int1, "other_name"))(getColumns(TestTable)) { (before, after) =>
        assert(before.map(_.name) === Vector("int1"))
        assert(after.map(_.name) === Vector("other_name"))
    } finally
      runMigration(tm.drop)

    assert(tm.renameColumn(_.int1, "other_name").reverse == tm.renameColumnFrom("other_name", _.int1))
  }
}
