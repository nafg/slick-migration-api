package slick
package migration.api

import java.sql.Types

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.control.NonFatal
import slick.jdbc.{HsqldbProfile, JdbcProfile, OracleProfile, PostgresProfile}
import slick.jdbc.meta._
import slick.lifted.AbstractTable
import com.typesafe.slick.testkit.util.JdbcTestDB
import org.scalactic.TypeCheckedTripleEquals
import org.scalactic.source.Position
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{BeforeAndAfterAll, Inside}
import org.scalatest.funsuite.AnyFunSuite


abstract class DbTest[P <: JdbcProfile](val tdb: JdbcTestDB {val profile: P})
                                       (implicit protected val dialect: Dialect[P])
  extends AnyFunSuite with Inside with BeforeAndAfterAll with TypeCheckedTripleEquals {

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

  val dateJdbcType = Types.DATE

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


  class TestTable(tag: Tag) extends Table[Long](tag, "TEST_TABLE") {
    def * = id

    val id = column[Long]("ID", O.AutoInc, O.PrimaryKey)

    val id1 = column[Int]("ID_1")
    val id2 = column[String]("ID_2", O.Length(254))

    val strWithDefault = column[String]("STR_WITH_DEFAULT", O.Default("abc"))

    val int1Nullable = column[Option[Int]]("INT1")

    val int1 = column[Int]("INT1")
    val int2 = column[Int]("INT2")
    val int3 = column[Int]("INT3")

    def other = column[Long]("OTHER")

    val index1 = index("INDEX1", int1)
    val index2 = index("INDEX2", (int2, int3), unique = true)

    val compoundPK = primaryKey("PRIMARY", (id1, id2)) // note mysql will always use the name "PRIMARY" anyway

    lazy val fk = foreignKey("FK_OTHER", other, TestTable)(_.id, ForeignKeyAction.NoAction, ForeignKeyAction.Cascade)
  }

  lazy val TestTable = TableQuery[TestTable]

  test("create, drop") {
    val tm = TableMigration(TestTable)
    val createTable = tm.create.addColumns(_.id, _.strWithDefault)

    withBeforeAndAfter(createTable)(getTables) { (before, after) =>
      try {
        val tableName = TestTable.baseTableRow.tableName
        inside(after filterNot before.contains) {
          case (_ @ MTable(MQName(_, _, `tableName`), "TABLE", _, _, _, _)) +: xs if xs.isEmpty =>
            val cols =
              runAction(getColumns(TestTable))
                .map(c => c.name -> c)
                .toMap
            assert(cols.keySet === Set("ID", "STR_WITH_DEFAULT"))
            val default = Some(columnDefaultFormat("abc"))
            val autoInc = Some(!profile.isInstanceOf[OracleProfile])
            inside(cols("ID")) {
              case MColumn(_, _, `longJdbcType`, _, _, _, _, Some(false), _, _, _, _, _, _, _, `autoInc`) =>
            }
            inside(cols("STR_WITH_DEFAULT")) {
              case MColumn(_, _, Types.VARCHAR, _, _, _, _, Some(false), _, `default`, _, _, _, _, _, Some(false)) =>
            }
        }

        assert(tm.create.reverse === tm.drop)

      } finally
        runMigration(tm.drop)

      assert(runAction(getTables) === before)
    }
  }

  test("auto incrementing") {
    val tm = TableMigration(TestTable)
    runMigration(tm.create.addColumns(_.id, _.strWithDefault))
    try {
      runAction(TestTable.map(_.strWithDefault) += "row 1")
      runAction(TestTable.map(_.strWithDefault) += "row 2")

        profile match {
          case _: HsqldbProfile => assert(runAction(TestTable.result).toSet === Set(0L, 1L))
          case _                => assert(runAction(TestTable.result).toSet === Set(1L, 2L))
        }
    } finally
      runMigration(tm.drop)
  }

  test("addColumns") {
    val tm = TableMigration(TestTable)
    runMigration(tm.create.addColumns(_.int2))

    try {
      val addOtherColumns = tm.addColumns(_.int1Nullable, _.strWithDefault)
      assert(addOtherColumns.reverse == tm.dropColumns(_.int1Nullable, _.strWithDefault))

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
        assert(i2(Some("INDEX1") -> false) === Vector(Some("INT1")))
        assert(i2(Some("INDEX2") -> true) === Vector(Some("INT2"), Some("INT3")))

        assert(createIndexes.reverse == tm.dropIndexes(_.index1, _.index2))

        withBeforeAndAfter(createIndexes.reverse)(indexes) { (_, i4) =>
          assert(i4.keys.flatMap(_._1).exists(Set("INDEX1", "INDEX2")) === false)
        }
      }
    finally
      runMigration(tm.drop)
  }

  test("rename") {
    val tm = TableMigration(TestTable)

    def tableNames = getTables.map(_.map(_.name.name).toSet)

    runMigration(tm.create.addColumns(_.int1))

    val renameMigration = tm.rename("OTHER_NAME")

    try
      withBeforeAndAfter(renameMigration)(tableNames) { (t2, t3) =>
        assert(t3 -- t2 === Set("OTHER_NAME"))
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
      assert(i2 === Set("INDEX1"))

      try
        withBeforeAndAfter(tm.renameIndex(_.index1, "OTHER_INDEX_NAME"))(indexNames) { (i3, i4) =>
          assert(i4 -- i3 === Set("OTHER_INDEX_NAME"))
        }
      finally
        runMigration(tm.drop)
    }
  }

  test("reverse") {
    val tm = TableMigration(TestTable)
    val createTable = tm.create.addColumns(_.id, _.strWithDefault)

    assert(createTable.sql.size === 1)
    assert(createTable.sql.head.contains("create table"))

    runMigration(createTable)

    val reversed = createTable.reverse

    assert(reversed.sql.size === 3)
    assert(reversed.sql.head.contains("ID"))
    assert(reversed.sql(1).contains("STR_WITH_DEFAULT"))
    assert(reversed.sql(2).contains("drop table"))

    try withBeforeAndAfter(reversed)(getTables) { (before, after) =>
      // Only postgres can remove all columns from a table
      if (this.profile.isInstanceOf[PostgresProfile]) {
        assert(before.contains("TEST_TABLE"))
        assert(!after.contains("TEST_TABLE"))
      }
    } catch {
      case _: Throwable =>
        if (!this.profile.isInstanceOf[PostgresProfile]) {
          runMigration(tm.drop)
        }
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
        assert(pks2(Some("PRIMARY")) === Vector("ID_1", "ID_2"))
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
          case Vector(MForeignKey(_, "ID", MQName(_, _, "TEST_TABLE"), "OTHER", _, `noActionReturns`, ForeignKeyAction.Cascade, Some("FK_OTHER"), _, _)) =>
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

    assert(tm.addColumns(_.id, _.int1).reverse == tm.dropColumns(_.id, _.int1))
  }

  test("alterColumnTypes") {
    val tm = TableMigration(TestTable)
    runMigration(tm.create.addColumns(_.column[java.sql.Date]("STR_WITH_DEFAULT")))

    try
      withBeforeAndAfter(tm.alterColumnTypes(_.strWithDefault))(getColumns(TestTable)) { (before, after) =>
        assert(before.map(_.sqlType) === Vector(dateJdbcType))
        assert(after.map(_.sqlType) === Vector(Types.VARCHAR))
      }
    finally
      runMigration(tm.drop)
  }

  test("alterColumnTypes with data") {
    val tm = TableMigration(TestTable)
    runMigration(tm.create.addColumns(_.column[java.sql.Date]("STR_WITH_DEFAULT")))
    runAction(TestTable.map(_.column[java.sql.Date]("STR_WITH_DEFAULT")) += new java.sql.Date(1))

    try
      withBeforeAndAfter(tm.alterColumnTypes(_.strWithDefault))(getColumns(TestTable)) { (before, after) =>
        assert(before.map(_.sqlType) === Vector(dateJdbcType))
        assert(after.map(_.sqlType) === Vector(Types.VARCHAR))
      }
    finally
      runMigration(tm.drop)
  }

  test("alterColumnDefaults") {
    val tm = TableMigration(TestTable)
    runMigration(tm.create.addColumns(_.column[String]("STR_WITH_DEFAULT", profile.columnOptions.Length(254))))

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
      withBeforeAndAfter(tm.renameColumn(_.int1, "OTHER_NAME"))(getColumns(TestTable)) { (before, after) =>
        assert(before.map(_.name) === Vector("INT1"))
        assert(after.map(_.name) === Vector("OTHER_NAME"))
    } finally
      runMigration(tm.drop)

    assert(tm.renameColumn(_.int1, "other_name").reverse == tm.renameColumnFrom("other_name", _.int1))
  }
}
