package scala.slick
package migration.api

import scala.slick.jdbc.JdbcBackend

trait Migration {
  def apply()(implicit session: JdbcBackend#Session): Unit
}

object Migration {
  implicit class MigrationConcat[M <: Migration](m: M) {
    def &[N <: Migration, O](n: N)(implicit ccm: CanConcatMigrations[M, N, O]): O = ccm.f(m, n)
  }
}

trait ReversibleMigration extends Migration {
  def reverse: Migration
}

class CanConcatMigrations[-A, -B, +C](val f: (A, B) => C)
class CanConcatMigrationsLow {
  implicit def default[A <: Migration, B <: Migration]: CanConcatMigrations[A, B, MigrationSeq] =
    new CanConcatMigrations({
      case (MigrationSeq(as @ _*), b) => MigrationSeq(as :+ b: _*)
      case (a, b)                     => MigrationSeq(a, b)
    })
}
object CanConcatMigrations extends CanConcatMigrationsLow {
  implicit def reversible[A <: ReversibleMigration, B <: ReversibleMigration]: CanConcatMigrations[A, B, ReversibleMigrationSeq] = new CanConcatMigrations({
    case (rms: ReversibleMigrationSeq, b) => new ReversibleMigrationSeq(rms.migrations :+ b: _*)
    case (a, b)                           => new ReversibleMigrationSeq(a, b)
  })
}

case class MigrationSeq(migrations: Migration*) extends Migration {
  final def apply()(implicit session: JdbcBackend#Session) = migrations foreach (_())
}

class ReversibleMigrationSeq(override val migrations: ReversibleMigration*) extends MigrationSeq(migrations: _*) with ReversibleMigration {
  def reverse = MigrationSeq(migrations.reverse.map(_.reverse): _*)
}

trait SqlMigration extends Migration {
  def sql: Seq[String]
  def apply()(implicit session: JdbcBackend#Session) = {
    val sq = sql
    session.withTransaction {
      session.withStatement() { st =>
        for(s <- sq)
          try st execute s
          catch {
            case e: java.sql.SQLException =>
              throw MigrationException(s"Could not execute sql: '$s'", e)
          }
      }
    }
  }
}

//TODO mechanism other than exceptions?
case class MigrationException(message: String, cause: Throwable) extends RuntimeException(message, cause)

object SqlMigration {
  def apply(sql: String*) = {
    def sql0 = sql
    new SqlMigration {
      def sql = sql0
    }
  }
}
