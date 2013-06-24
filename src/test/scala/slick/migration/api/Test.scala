package scala.slick
package migration.api

import org.scalatest.fixture
import org.scalatest.Inside
import org.scalatest.matchers.ShouldMatchers


import com.typesafe.slick.testkit.util._

import scala.slick.driver._

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

    object table1 extends Table[Long]("table1") {
      def id = column[Long]("id", O.NotNull, O.AutoInc)
      def * = id
    }

    val tables = MTable.getTables

    val before = tables.list

    val createTable = CreateTable(table1)(_.id)

    createTable()

    val after = tables.list

    inside(after filterNot before.contains) {
      case (table @ MTable(MQName(_, _, table1.tableName), "TABLE", _, _, _, _)) :: Nil =>
        table.getColumns.list.map {
          case col => (col.column, col.typeName, col.nullable, col.isAutoInc)
        } should equal (List(
          ("id", "BIGINT", Some(false), Some(true))
        ))
    }

    createTable.reverse should equal (DropTable(table1))

    createTable.reverse()

    tables.list should equal (before)
  }
}
