package scala.slick
package migration.api

import org.scalatest.fixture
import org.scalatest.Inside
import org.scalatest.matchers.ShouldMatchers


import com.typesafe.slick.testkit.util._

import scala.slick.driver._
import scala.slick.lifted.ForeignKeyAction

import jdbc.meta._

object H2Mem extends JdbcTestDB("h2mem") {
  type Driver = H2Driver.type
  val driver = H2Driver
  val url = "jdbc:h2:mem:test1"
  val jdbcDriver = "org.h2.Driver"
  override def isPersistent = false
  override lazy val capabilities = driver.capabilities + TestDB.plainSql + TestDB.plainSqlWide
}

trait DbFixture { this: fixture.Suite =>
  class FixtureParam(driver: JdbcDriver, val session: JdbcDriver.simple.Session) extends Migrations(driver)
  implicit def sessForFP(implicit fp: FixtureParam): JdbcDriver.simple.Session = fp.session

  val dbs = List(H2Mem)

  def withFixture(test: OneArgTest) = for(tdb <- dbs) tdb.driver match {
    case driver: JdbcDriver =>
      tdb.cleanUpBefore()
      val db = tdb.createDB()
      try db.withSession { session: driver.simple.Session =>
        test(new FixtureParam(driver, session))
      } finally tdb.cleanUpAfter()
  }
}

class Test extends fixture.FunSuite with ShouldMatchers with Inside with DbFixture {

  test("& returns the right type and doesn't keep nesting") { fix =>
    import fix._
    val m = new Migration {
      def apply()(implicit s: driver.simple.Session) = ???
    }
    m & m & m should equal (MigrationSeq(m, m, m))

    val rm = new ReversibleMigration {
      def apply()(implicit s: driver.simple.Session) = ???
      def reverse = ???
    }

    val rms = rm & rm & rm
    val rms1: ReversibleMigrationSeq = rms // verify the type
    rms should equal (new ReversibleMigrationSeq(rm, rm, rm))
  }

  test("CreateTable, DropTable") { implicit fix =>
    import fix._
    import driver.simple._

    object table1 extends Table[(Long, Int)]("table1") {
      def id = column[Long]("id", O.NotNull, O.AutoInc)
      def col1 = column[Int]("col1", O.Default(10))
      def * = id ~ col1
    }

    val tables = MTable.getTables

    val before = tables.list

    val createTable = CreateTable(table1)(_.id, _.col1)

    createTable()

    val after = tables.list

    inside(after filterNot before.contains) {
      case (table @ MTable(MQName(_, _, table1.tableName), "TABLE", _, _, _, _)) :: Nil =>
        import java.sql.Types
        val cols = table.getColumns.list
        cols.map {
          case col => (col.column, col.sqlType, col.nullable, col.isAutoInc)
        } should equal (List(
          ("id", Types.BIGINT, Some(false), Some(true)),
          ("col1", Types.INTEGER, Some(false), Some(false))
        ))
        cols.find(_.column == "col1").flatMap(_.columnDef) should equal (Some("10"))
    }

    createTable.reverse should equal (DropTable(table1))

    createTable.reverse()

    tables.list should equal (before)
  }

  test("CreateForeignKey, DropForeignKey") { implicit fix =>
    import fix._
    import driver.simple._

    object table1 extends Table[(Long, Long)]("table1") {
      def id = column[Long]("id", O.PrimaryKey)
      def other = column[Long]("other")

      def * = id ~ other
      // lazy val so equality works
      lazy val fk = foreignKey("fk_other", other, table2)(_.id, ForeignKeyAction.Cascade, ForeignKeyAction.Cascade)
    }
    object table2 extends Table[Long]("table2") {
      def id = column[Long]("id", O.PrimaryKey)
      def * = id
    }

    CreateTable(table1)(_.id, _.other)()
    CreateTable(table2)(_.id)()

    def fks = MTable.getTables.to[Set] map { t =>
      (
        t.name.name,
        t.getImportedKeys.list map { fk =>
          (fk.pkTable.name, fk.pkColumn, fk.fkTable.name, fk.fkColumn, fk.updateRule, fk.deleteRule, fk.fkName)
        }
      )
    }

    val before = fks

    before should equal (Set(
      ("table1", Nil),
      ("table2", Nil)
    ))

    val createForeignKey = CreateForeignKey(table1.fk)
    createForeignKey()

    fks should equal (Set(
      ("table1", ("table2", "id", "table1", "other", ForeignKeyAction.Cascade, ForeignKeyAction.Cascade, Some("fk_other")) :: Nil),
      ("table2", Nil)
    ))

    val dropForeignKey = createForeignKey.reverse
    inside(dropForeignKey.migrations.toList) {
      case DropForeignKey(fk) :: Nil =>
        Seq(fk) should equal (table1.fk.fks)
    }

    dropForeignKey()

    fks should equal (before)
  }
}
