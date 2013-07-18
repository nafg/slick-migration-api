package scala.slick
package migration.api

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import scala.slick.driver.H2Driver

class MigrationSeqTest extends FunSuite with ShouldMatchers {
  test("& returns the right type and doesn't keep nesting") {
    object migrations extends Migrations(H2Driver)
    import migrations._
    val m = new Migration {
      def apply()(implicit s: driver.simple.Session) = ???
    }
    m & m & m should equal (MigrationSeq(m, m, m))

    val rm = new ReversibleMigration {
      def apply()(implicit s: driver.simple.Session) = ???
      def reverse = ???
    }

    val rms = rm & rm & rm
    implicitly[rms.type <:< ReversibleMigrationSeq]
    rms should equal (new ReversibleMigrationSeq(rm, rm, rm))
  }
}
