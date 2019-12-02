package slick
package migration.api

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MigrationSeqTest extends AnyFunSuite with Matchers {
  test("& returns the right type and doesn't keep nesting") {

    import slick.jdbc.H2Profile.api._

    val m = Migration.empty
    m & m & m should equal (MigrationSeq(m, m, m))

    val s1 = SqlMigration("select 1")
    val s2 = SqlMigration("select 2")
    val s3 = SqlMigration("select 3")
    val seq = s1 & s2 & s3
    seq should equal (MigrationSeq(s1, s2, s3))

    val rm: ReversibleMigration = new ReversibleMigration {
      override def apply() = DBIO.successful(())
      def reverse: ReversibleMigration = this
    }

    val rms = rm & rm & rm
    implicitly[rms.type <:< ReversibleMigrationSeq]
    rms should equal (new ReversibleMigrationSeq(rm, rm, rm))
  }
}
