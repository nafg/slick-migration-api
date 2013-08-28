package scala.slick
package migration.api

import java.sql.{SQLException, Types}

import scala.slick.jdbc.JdbcBackend
import scala.slick.jdbc.meta.{MIndexInfo, MPrimaryKey, MQName, MTable}
import scala.slick.lifted.ForeignKeyAction

import org.scalatest.{BeforeAndAfterAll, FunSuite, Inside}
import org.scalatest.matchers.ShouldMatchers

import com.typesafe.slick.testkit.util.JdbcTestDB

abstract class BasicDbTest[Drv <: driver.JdbcDriver](val tdb: JdbcTestDB { type Driver <: Drv })(implicit hasDialect: HasDialect[Drv]) extends FunSuite with ShouldMatchers with Inside with BeforeAndAfterAll {

  implicit lazy val session = tdb.createDB.createSession

  lazy val driver: Drv = tdb.driver

  object migrations extends Migrations[Drv](driver)

  import migrations._
  import driver.simple._

  override def beforeAll() = tdb.cleanUpBefore()
  override def afterAll() = {
    session.close()
    tdb.cleanUpAfter()
  }

  val catalog, schema = Option("")

  def noActionReturns: ForeignKeyAction = ForeignKeyAction.NoAction

  def getTables(implicit session: JdbcBackend#Session) = MTable.getTables(catalog, schema, None, None).list
  def getTable(name: String)(implicit session: JdbcBackend#Session) =
    getTables.find(_.name.name == name)

  def longJdbcType = Types.BIGINT

  /**
   * How JDBC metadata returns a column's default string value
   */
  def columnDefaultFormat(s: String) = s"'$s'"

  test("CreateTable, DropTable") {
    object table1 extends Table[(Long, String)]("table1") {
      def id = column[Long]("id", O.NotNull, O.AutoInc, O.PrimaryKey)
      def col1 = column[String]("col1", O.Default("abc"))
      def * = id ~ col1
    }

    val before = getTables

    val createTable = CreateTable(table1)(_.id, _.col1)

    createTable()

    try {
      val after = getTables

      inside(after filterNot before.contains) {
        case (table @ MTable(MQName(_, _, table1.tableName), "TABLE", _, _, _, _)) :: Nil =>
          val cols = table.getColumns.list
          cols.map(col => (col.column, col.sqlType, col.nullable)) should equal (List(
            ("id", longJdbcType, Some(false)),
            ("col1", Types.VARCHAR, Some(false))
          ))
          val autoinc: Map[String, Boolean] = cols.flatMap(col => col.isAutoInc.map(col.column -> _)).toMap
          autoinc.get("id") foreach (_ should equal (true))
          autoinc.get("col1") foreach (_ should equal (false))
          cols.find(_.column == "col1").flatMap(_.columnDef).foreach(_ should equal (columnDefaultFormat("abc")))
      }

      createTable.reverse should equal (DropTable(table1))

    } finally {
      createTable.reverse()
    }

    getTables should equal (before)
  }

  test("AddColumn") {
    object table5 extends Table[(Long, String)]("table5") {
      def col1 = column[Long]("col1")
      def col2 = column[String]("col2", O.Default(""))
      def * = col1 ~ col2
    }

    CreateTable(table5)(_.col1)()

    def columnsCount = getTable("table5").map(_.getColumns.list.length)

    columnsCount should equal (Some(1))

    val addColumn = AddColumn(table5)(_.col2)
    addColumn()

    // note this doesn't actually compare the second argument
    addColumn.reverse should equal (DropColumn(table5)(_.col2))

    columnsCount should equal (Some(2))

    DropTable(table5)()
  }

  test("CreateIndex, DropIndex") {
    object table4 extends Table[(Long, Int, Int, Int)]("table4") {
      def id = column[Long]("id")
      def col1 = column[Int]("col1")
      def col2 = column[Int]("col2")
      def col3 = column[Int]("col3")
      def * = id ~ col1 ~ col2 ~ col3
      val index1 = index("index1", col1)
      val index2 = index("index2", col2 ~ col3, true)
    }

    def indexList = getTable("table4").map(_.getIndexInfo().list) getOrElse Nil
    def indexes = indexList
      .groupBy(i => (i.indexName, !i.nonUnique))
      .mapValues {
        _ collect {
          case MIndexInfo(MQName(_, _, "table4"), _, _, _, _, seq, col, _, _, _, _) =>
            (seq, col)
        }
      }

    CreateTable(table4)(_.id, _.col1, _.col2, _.col3)()

    val createIndexes = CreateIndex(table4.index1) & CreateIndex(table4.index2)
    createIndexes()

    indexes(Some("index1") -> false) should equal (List((1, Some("col1"))))
    indexes(Some("index2") -> true) should equal (List((1, Some("col2")), (2, Some("col3"))))

    createIndexes.reverse should equal (DropIndex(table4.index2) & DropIndex(table4.index1))

    createIndexes.reverse()

    DropTable(table4)()
  }

  test("RenameTable/Index") {
    class table(name: String) extends Table[Long](name) {
      def col1 = column[Long]("col1")
      def * = col1
      val index1 = index("oldIndexName", col1)
    }
    object oldname extends table("oldname")
    object table7 extends table("table7")

    CreateTable(oldname)(_.col1).withIndexes(_.index1)()

    def tables = getTables.map(_.name.name).filterNot(_ == "oldIndexName")
    def indexes = getTables.flatMap(_.getIndexInfo().list.flatMap(_.indexName))

    tables should equal (List("oldname"))
    indexes should equal (List("oldIndexName"))

    RenameTable(oldname, "table7")()
    tables should equal (List("table7"))

    RenameIndex(table7.index1, "index1")()
    indexes should equal (List("index1"))

    DropTable(table7)()
  }
}

abstract class DbTest[Drv <: driver.JdbcDriver](tdb: JdbcTestDB { type Driver <: Drv })(implicit hasDialect: HasDialect[Drv]) extends BasicDbTest[Drv](tdb) {

  import migrations._
  import driver.simple._

  test("CreatePrimaryKey, DropPrimaryKey") {
    def pkList = getTable("table8").map(_.getPrimaryKeys.list) getOrElse Nil
    def pks = pkList
      .groupBy(_.pkName)
      .mapValues {
        _ collect { case MPrimaryKey(MQName(_, _, "table8"), col, seq, _) => (seq, col) }
      }

    object table8 extends Table[(Long, String)]("table8") {
      def id = column[Long]("id")
      def stringId = column[String]("stringId")
      def * = id ~ stringId
      def pk = primaryKey("PRIMARY", id ~ stringId)  // note mysql will always use this name anyway
    }

    CreateTable(table8)(_.id, _.stringId)()

    val before = pks

    before.get(Some("PRIMARY")) should equal (None)

    try {
      val createPrimaryKey = CreatePrimaryKey(table8)(_.pk)
      createPrimaryKey()

      pks(Some("PRIMARY")) should equal (List(1 -> "id", 2 -> "stringId"))

      //TODO this doesn't do much since case classes only compare the first parameter list
      createPrimaryKey.reverse should equal (DropPrimaryKey(table8)(_.pk))
      createPrimaryKey.reverse()

      pks should equal (before)
    } finally DropTable(table8)()
  }

  test("CreateForeignKey, DropForeignKey") {
    object table2 extends Table[(Long, Long)]("table2") {
      def id = column[Long]("id", O.PrimaryKey)
      def other = column[Long]("other")

      def * = id ~ other
      // not a def, so equality works
      lazy val fk = foreignKey("fk_other", other, table3)(_.id, ForeignKeyAction.NoAction, ForeignKeyAction.Cascade)
    }
    object table3 extends Table[Long]("table3") {
      def id = column[Long]("id", O.PrimaryKey)
      def * = id
    }

    try {
      CreateTable(table2)(_.id, _.other)()
      CreateTable(table3)(_.id)()

      def fks = getTables.to[Set] map { t =>
        (
          t.name.name,
          t.getExportedKeys.list map { fk =>
            (fk.pkTable.name, fk.pkColumn, fk.fkTable.name, fk.fkColumn, fk.updateRule, fk.deleteRule, fk.fkName)
          }
        )
      }

      val before = fks

      before should equal (Set(
        ("table2", Nil),
        ("table3", Nil)
      ))

      val createForeignKey = CreateForeignKey(table2.fk)
      createForeignKey()

      fks should equal (Set(
        ("table2", Nil),
        ("table3", ("table3", "id", "table2", "other", noActionReturns, ForeignKeyAction.Cascade, Some("fk_other")) :: Nil)
      ))

      val dropForeignKey = createForeignKey.reverse
      inside(dropForeignKey.migrations.toList) {
        case DropForeignKey(fk) :: Nil =>
          Seq(fk) should equal (table2.fk.fks)
      }

      dropForeignKey()

      fks should equal (before)
    } finally {
      DropTable(table2)()
      DropTable(table3)()
    }
  }

  test("DropColumn") {
    object table11 extends Table[(Long, String)]("table11") {
      def col1 = column[Long]("col1")
      def col2 = column[String]("col2", O.Default(""))
      def * = col1 ~ col2
    }

    CreateTable(table11)(_.col1, _.col2)()

    def columnsCount = getTable("table11").map(_.getColumns.list.length)

    columnsCount should equal (Some(2))

    DropColumn(table11)(_.col2)()

    columnsCount should equal (Some(1))

    DropTable(table11)()
  }

  test("AlterColumnType") {
    object table6 extends Table[String]("table6") {
      def tmpOldId = column[java.sql.Date]("id")
      def id = column[String]("id", O.Default(""))
      def * = id
    }

    CreateTable(table6)(_.tmpOldId)()

    def columnTypes = getTable("table6").map(_.getColumns.list.map(_.sqlType)) getOrElse Nil

    try {
      columnTypes.toList should equal (List(Types.DATE))
      AlterColumnType(table6)(_.id)()
      columnTypes.toList should equal (List(Types.VARCHAR))
    } finally DropTable(table6)()
  }

  test("AlterColumnDefault") {
    object table9 extends Table[String]("table9") {
      def tmpOldId = column[String]("id")
      def id = column[String]("id", O.Default("abc"))
      def * = id
    }

    CreateTable(table9)(_.tmpOldId)()

    def columns = getTable("table9").map(_.getColumns.list.map(_.columnDef)) getOrElse Nil

    try {
      columns.toList should equal (List((None)))
      AlterColumnDefault(table9)(_.id)()
      columns.toList should equal (List((Some(columnDefaultFormat("abc")))))
    } finally DropTable(table9)()
  }

  test("AlterColumnNullability") {
    object table10 extends Table[String]("table10") {
      def tmpOldId = column[String]("id", O.Nullable)
      def id = column[String]("id", O.NotNull)
      def * = id
    }

    CreateTable(table10)(_.tmpOldId)()

    try {
      table10.tmpOldId.insert(null: String)
      Query(table10).delete

      AlterColumnNullability(table10)(_.id)()

      intercept[SQLException] {
        table10.id.insert(null: String)
      }
    } finally DropTable(table10)()
  }

  test("RenameColumn") {
    object table12 extends Table[Long]("table12") {
      def col1 = column[Long]("oldname")
      def * = col1
    }

    def columns = getTables.flatMap(_.getColumns.list.map(_.column))

    CreateTable(table12)(_.col1)()
    columns should equal (List("oldname"))

    RenameColumn(table12)(_.col1, _.column[Long]("col1"))()
    columns should equal (List("col1"))

    DropTable(table12)()
  }
}
