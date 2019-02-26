package slick
package migration.api

import java.sql.{SQLException, Types}

import slick.jdbc.{JdbcBackend, JdbcProfile}
import slick.jdbc.meta.{MIndexInfo, MPrimaryKey, MQName, MTable}

import com.typesafe.slick.testkit.util.JdbcTestDB
import org.scalatest.{BeforeAndAfterAll, FunSuite, Inside, Matchers}

abstract class DbTest[P <: JdbcProfile](val tdb: JdbcTestDB { val profile: P })(implicit protected val dialect: Dialect[P])
  extends FunSuite
  with Matchers
  with Inside
  with BeforeAndAfterAll {

  implicit lazy val session: JdbcBackend#Session = tdb.createDB.createSession

  lazy val profile: P = tdb.profile

  import profile.api._

  override def beforeAll(): Unit = tdb.cleanUpBefore()
  override def afterAll(): Unit = {
    session.close()
    tdb.cleanUpAfter()
  }

  val catalog, schema = Option("")

  def noActionReturns: ForeignKeyAction = ForeignKeyAction.NoAction

  def getTables(implicit session: JdbcBackend#Session): Vector[MTable] =
    tdb.blockingRunOnSession(implicit e => MTable.getTables(catalog, schema, None, Some(Seq("TABLE"))))
  def getTable(name: String)(implicit session: JdbcBackend#Session): Option[MTable] =
    getTables.find(_.name.name == name)

  def longJdbcType = Types.BIGINT

  /**
   * How JDBC metadata returns a column's default string value
   */
  def columnDefaultFormat(s: String) = s"'$s'"

  test("create, drop") {
    class Table1(tag: Tag) extends Table[(Long, String)](tag, "table1") {
      def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
      def col1 = column[String]("col1", O.Default("abc"), O.Length(254))
      def * = (id, col1)
    }
    val table1 = TableQuery[Table1]

    val before = getTables

    val tm = TableMigration(table1)
    val createTable = tm.create.addColumns(_.id, _.col1)

    tdb.blockingRunOnSession(implicit ec => createTable())

    try {
      val after = getTables
      val tableName = table1.baseTableRow.tableName
      inside(after filterNot before.contains) {
        case (table @ MTable(MQName(_, _, `tableName`), "TABLE", _, _, _, _)) +: xs if xs.isEmpty =>
          val cols = tdb.blockingRunOnSession(implicit ec => table.getColumns)
          cols.map(col => (col.name, col.sqlType, col.nullable)) should equal (Vector(
            ("id", longJdbcType, Some(false)),
            ("col1", Types.VARCHAR, Some(false))
          ))
          val autoinc: Map[String, Boolean] = cols.flatMap(col => col.isAutoInc.map(col.name -> _)).toMap
          autoinc.get("id") foreach (_ should equal (true))
          autoinc.get("col1") foreach (_ should equal (false))
          cols.find(_.name == "col1").flatMap(_.columnDef).foreach(_ should equal (columnDefaultFormat("abc")))
      }

      tm.create.reverse should equal (tm.drop)

    } finally
      tdb.blockingRunOnSession(implicit ec => tm.drop())

    getTables should equal (before)
  }

  test("addColumns") {
    class Table5(tag: Tag) extends Table[(Long, String, Option[Int])](tag, "table5") {
      def col1 = column[Long]("col1")
      def col2 = column[String]("col2", O.Default(""), O.Length(254))
      def col3 = column[Option[Int]]("col3")
      def * = (col1, col2, col3)
    }
    val table5 = TableQuery[Table5]

    val tm = TableMigration(table5)
    tdb.blockingRunOnSession(implicit ec => tm.create.addColumns(_.col1)())

    def columnsCount = getTable("table5").map { table =>
      tdb.blockingRunOnSession(implicit ec => table.getColumns).length
    }

    try {
      columnsCount should equal (Some(1))

      val addColumn = tm.addColumns(_.col2, _.col3)
      addColumn.reverse should equal (tm.dropColumns(_.col3, _.col2))

      tdb.blockingRunOnSession(implicit ec => addColumn())

      columnsCount should equal (Some(3))
    } finally
      tdb.blockingRunOnSession(implicit ec => tm.drop())
  }

  test("addIndexes, dropIndexes") {
    class Table4(tag: Tag) extends Table[(Long, Int, Int, Int)](tag, "table4") {
      def id = column[Long]("id")
      def col1 = column[Int]("col1")
      def col2 = column[Int]("col2")
      def col3 = column[Int]("col3")
      def * = (id, col1, col2, col3)
      val index1 = index("index1", col1)
      val index2 = index("index2", (col2, col3), unique = true)
    }
    val table4 = TableQuery[Table4]

    def indexList = getTable("table4").map { table =>
      tdb.blockingRunOnSession(implicit ec => table.getIndexInfo())
    } getOrElse Vector.empty
    def indexes = indexList
      .groupBy(i => (i.indexName, !i.nonUnique))
      .mapValues {
        _ collect {
          case MIndexInfo(MQName(_, _, "table4"), _, _, _, _, seq, col, _, _, _, _) =>
            (seq, col)
        }
      }

    val tm = TableMigration(table4)
    tdb.blockingRunOnSession { implicit ec =>
      tm.create.addColumns(_.id, _.col1, _.col2, _.col3)()
    }

    try {
      val createIndexes = tm.addIndexes(_.index1, _.index2)
      tdb.blockingRunOnSession(implicit ec => createIndexes())

      indexes(Some("index1") -> false) should equal (Vector((1, Some("col1"))))
      indexes(Some("index2") -> true) should equal (Vector((1, Some("col2")), (2, Some("col3"))))

      createIndexes.reverse should equal (tm.dropIndexes(_.index2, _.index1))

      tdb.blockingRunOnSession(implicit ec => createIndexes.reverse())

      indexes.keys.flatMap(_._1).exists(Set("index1", "index2")) should equal (false)
    } finally
      tdb.blockingRunOnSession(implicit ec => tm.drop())
  }

  test("rename, renameIndex") {
    def allTables = getTables.map(_.name.name).filterNot(_ == "oldIndexName")
    def indexes = for {
      tables <- getTables
      indexes <- tdb.blockingRunOnSession(implicit e => tables.getIndexInfo())
      name <- indexes.indexName
    } yield name

    val originalState = allTables.toSet
    def tables = allTables.filter(!originalState.contains(_))

    class Table7Base(tag: Tag, name: String) extends Table[Long](tag, name) {
      def col1 = column[Long]("col1")
      def * = col1
      val index1 = index("oldIndexName", col1)
    }
    class OldName(tag: Tag) extends Table7Base(tag, "old_name")
    class Table7(tag: Tag) extends Table7Base(tag, "table7")
    val oldName = TableQuery[OldName]
    val table7 = TableQuery[Table7]

    val tm = TableMigration(oldName)
    tdb.blockingRunOnSession { implicit ec =>
      tm.create.addColumns(_.col1).addIndexes(_.index1)()
    }

    tables should equal (Vector("old_name"))
    indexes should equal (Vector("oldIndexName"))

    tdb.blockingRunOnSession(implicit ec => tm.rename("table7")())
    tables should equal (Vector("table7"))

    val tm7 = TableMigration(table7)
    try {
      tdb.blockingRunOnSession { implicit ec =>
        tm7.renameIndex(_.index1, "index1")()
      }
      indexes should equal (Vector("index1"))
    } finally
      tdb.blockingRunOnSession(implicit ec => tm7.drop())

    tm.rename("table7").reverse should equal (tm7.rename("old_name"))
  }
}

trait CompleteDbTest { this: DbTest[_ <: JdbcProfile] =>
  import profile.api._

  test("addPrimaryKeys, dropPrimaryKeys") {
    def pkList = getTable("table8").map { table =>
      tdb.blockingRunOnSession(implicit e => table.getPrimaryKeys)
    } getOrElse Vector.empty
    def pks = pkList
      .groupBy(_.pkName)
      .mapValues {
        _ collect { case MPrimaryKey(MQName(_, _, "table8"), col, seq, _) => (seq, col) }
      }

    class Table8(tag: Tag) extends Table[(Long, String)](tag, "table8") {
      def id = column[Long]("id")
      def stringId = column[String]("stringId", O.Length(254))
      def * = (id, stringId)
      def pk = primaryKey("PRIMARY", (id, stringId))  // note mysql will always use the name "PRIMARY" anyway
    }
    val table8 = TableQuery[Table8]

    val tm = TableMigration(table8)
    tdb.blockingRunOnSession {
      implicit ec => tm.create.addColumns(_.id, _.stringId)()
    }

    val before = pks

    before.get(Some("PRIMARY")) should equal (None)

    try {
      tdb.blockingRunOnSession(implicit ec => tm.addPrimaryKeys(_.pk)())

      pks(Some("PRIMARY")) should equal (Vector(1 -> "id", 2 -> "stringId"))

      tm.addPrimaryKeys(_.pk).reverse should equal (tm.dropPrimaryKeys(_.pk))

      tdb.blockingRunOnSession(implicit ec => tm.dropPrimaryKeys(_.pk)())

      pks should equal (before)
    } finally
      tdb.blockingRunOnSession(implicit ec => tm.drop())
  }

  test("addForeignKeys, dropForeignKeys") {
    def allTables = getTables
    val originalState = allTables.toSet
    def tables = allTables.filter(!originalState.contains(_))

    class Table3(tag: Tag) extends Table[Long](tag, "table3") {
      def id = column[Long]("id", O.PrimaryKey)
      def * = id
    }
    val table3 = TableQuery[Table3]
    class Table2(tag: Tag) extends Table[(Long, Long)](tag, "table2") {
      def id = column[Long]("id", O.PrimaryKey)
      def other = column[Long]("other")

      def * = (id, other)
      // not a def, so that equality works
      lazy val fk = foreignKey("fk_other", other, table3)(_.id, ForeignKeyAction.NoAction, ForeignKeyAction.Cascade)
    }
    val table2 = TableQuery[Table2]

    val tm2 = TableMigration(table2)
    val tm3 = TableMigration(table3)
    try {
      tdb.blockingRunOnSession(implicit ec => tm2.create.addColumns(_.id, _.other)())
      tdb.blockingRunOnSession(implicit ec => tm3.create.addColumns(_.id)())

      def tableFks = tables.to[Set] map { t =>
        val name = t.name.name
        val fks = tdb.blockingRunOnSession(implicit e => t.getExportedKeys) map { fk =>
          (fk.pkTable.name, fk.pkColumn, fk.fkTable.name, fk.fkColumn, fk.updateRule, fk.deleteRule, fk.fkName)
        }
        name -> fks
      }

      val before = tableFks

      before should equal (Set(
        ("table2", Vector.empty),
        ("table3", Vector.empty)
      ))

      val createForeignKey = tm2.addForeignKeys(_.fk)
      tdb.blockingRunOnSession(implicit ec => createForeignKey())

      tableFks should equal (Set(
        ("table2", Vector.empty),
        ("table3", Vector(("table3", "id", "table2", "other", noActionReturns, ForeignKeyAction.Cascade, Some("fk_other"))))
      ))

      val dropForeignKey = createForeignKey.reverse

      dropForeignKey.actions.collect { case TableMigration.Action.DropForeignKey(fk) => fk } should equal(table2.baseTableRow.fk.fks.toList)
      dropForeignKey should equal (tm2.dropForeignKeys(_.fk))

      tdb.blockingRunOnSession(implicit ec => dropForeignKey())

      tableFks should equal (before)
    } finally {
      tdb.blockingRunOnSession(implicit ec => tm2.drop())
      tdb.blockingRunOnSession(implicit ec => tm3.drop())
    }
  }

  test("dropColumns") {
    class Table11(tag: Tag) extends Table[(Long, String)](tag, "table11") {
      def col1 = column[Long]("col1")
      def col2 = column[String]("col2", O.Default(""), O.Length(254))
      def * = (col1, col2)
    }
    val table11 = TableQuery[Table11]

    val tm = TableMigration(table11)
    tdb.blockingRunOnSession(implicit ec => tm.create.addColumns(_.col1, _.col2)())

    def columnsCount = getTable("table11").map { table =>
      tdb.blockingRunOnSession(implicit e => table.getColumns).length
    }

    try {
      columnsCount should equal (Some(2))

      tdb.blockingRunOnSession(implicit ec => tm.dropColumns(_.col2)())

      columnsCount should equal (Some(1))
    } finally
      tdb.blockingRunOnSession(implicit ec => tm.drop())

    tm.addColumns(_.col1, _.col2).reverse should equal (tm.dropColumns(_.col2, _.col1))
  }

  test("alterColumnTypes") {
    class Table6(tag: Tag) extends Table[String](tag, "table6") {
      def tmpOldId = column[java.sql.Date]("id")
      def id = column[String]("id", O.Default(""), O.Length(254))
      def * = id
    }
    val table6 = TableQuery[Table6]

    val tm = TableMigration(table6)
    tdb.blockingRunOnSession(implicit ec => tm.create.addColumns(_.tmpOldId)())

    def columnTypes = getTable("table6").map { table =>
      tdb.blockingRunOnSession(implicit e => table.getColumns).map(_.sqlType)
    } getOrElse Vector.empty

    try {
      columnTypes should equal (Vector(Types.DATE))
      tdb.blockingRunOnSession(implicit ec => tm.alterColumnTypes(_.id)())
      columnTypes should equal (Vector(Types.VARCHAR))
    } finally
      tdb.blockingRunOnSession(implicit ec => tm.drop())
  }

  test("alterColumnDefaults") {
    class Table9(tag: Tag) extends Table[String](tag, "table9") {
      def tmpOldId = column[String]("id", O.Length(254))
      def id = column[String]("id", O.Default("abc"), O.Length(254))
      def * = id
    }
    val table9 = TableQuery[Table9]

    val tm = TableMigration(table9)
    tdb.blockingRunOnSession(implicit ec => tm.create.addColumns(_.tmpOldId)())

    def columns = getTable("table9").map { table =>
      tdb.blockingRunOnSession(implicit e => table.getColumns).map(_.columnDef)
    } getOrElse Vector.empty

    try {
      columns should equal (Vector(None))
      tdb.blockingRunOnSession(implicit ec => tm.alterColumnDefaults(_.id)())
      columns should equal (Vector(Some(columnDefaultFormat("abc"))))
    } finally
      tdb.blockingRunOnSession(implicit ec => tm.drop())
  }

  test("alterColumnNulls") {
    class Table10(tag: Tag) extends Table[String](tag, "table10") {
      def tmpOldId = column[Option[String]]("id", O.Length(254))
      def id = column[String]("id", O.Length(254))
      def * = id
    }
    val table10 = TableQuery[Table10]

    val tm = TableMigration(table10)
    tdb.blockingRunOnSession(implicit ec => tm.create.addColumns(_.tmpOldId)())

    try {
      tdb.blockingRunOnSession { implicit e =>
        for {
          _ <- table10.map(_.tmpOldId) += None
          _ <- table10.delete
        } yield ()
      }

      tdb.blockingRunOnSession(implicit ec => tm.alterColumnNulls(_.id)())

      intercept[SQLException] {
        tdb.blockingRunOnSession { implicit e =>
          table10.map(_.id) += (null: String)
        }
      }
    } finally
      tdb.blockingRunOnSession(implicit ec => tm.drop())
  }

  test("renameColumn") {
    class Table12(tag: Tag) extends Table[Long](tag, "table12") {
      def col1 = column[Long]("old_name")
      def * = col1
    }
    val table12 = TableQuery[Table12]

    def columns = for {
      table <- getTable("table12").toVector
      column <- tdb.blockingRunOnSession(implicit e => table.getColumns)
    } yield column.name

    val tm = TableMigration(table12)
    tdb.blockingRunOnSession(implicit ec => tm.create.addColumns(_.col1)())

    try {
      columns should equal (Vector("old_name"))

      tdb.blockingRunOnSession(implicit ec => tm.renameColumn(_.col1, "col1")())

      columns should equal (Vector("col1"))
    } finally
      tdb.blockingRunOnSession(implicit ec => tm.drop())

    tm.renameColumn(_.col1, "col1").reverse should equal (tm.renameColumn(_.column[Long]("col1"), "old_name"))
  }
}
