package slick
package migration.api

import org.scalatest.Matchers
import org.scalatest.FunSuite
import slick.jdbc.JdbcBackend

import scala.concurrent.{Future, ExecutionContext}

class MigrationSeqTest extends FunSuite with Matchers {
  test("& returns the right type and doesn't keep nesting") {

    import slick.driver.H2Driver.api._

    val m = new Migration {
      override def run(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[Unit] =
        Future.successful(())
    }
    m & m & m should equal (MigrationSeq(m, m, m))

    val s1 = SqlMigration(sql"select 1".as[Int])
    val s2 = SqlMigration(sql"select 2".as[Int])
    val s3 = SqlMigration(sql"select 3".as[Int])
    val seq = s1 & s2 & s3
    seq should equal (MigrationSeq(s1, s2, s3))
    seq.statements should equal (Seq("select 1", "select 2", "select 3"))

    val rm = new ReversibleMigration {
      override def run(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[Unit] =
        Future.successful(())
      def reverse = this
    }

    val rms = rm & rm & rm
    implicitly[rms.type <:< ReversibleMigrationSeq]
    rms should equal (new ReversibleMigrationSeq(rm, rm, rm))
  }
}
