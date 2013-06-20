package scala.slick
package migration.api

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class Test extends FunSuite with ShouldMatchers {
  val migrations = new Migrations(driver.PostgresDriver)
  import migrations._

  test("& returns the right type and doesn't keep nesting") {
    val m = new Migration {
      def apply()(implicit s: driver.backend.Session) = ???
    }
    m & m & m should equal (MigrationSeq(m, m, m))

    val rm = new ReversibleMigration {
      def apply()(implicit s: driver.backend.Session) = ???
      def reverse = ???
    }

    val rms = rm & rm & rm
    val rms1: ReversibleMigrationSeq = rms // verify the type
    rms should equal (new ReversibleMigrationSeq(rm, rm, rm))
  }
  test("test") {
    
    val s1, s2 = SqlMigration("hello")
    // println(s1 & s2)
  }
}
