package slick
package migration.api

import slick.dbio.{ FailureAction, SuccessAction, DBIO }
import slick.jdbc.JdbcBackend
import slick.profile.SqlAction

import scala.concurrent.{ ExecutionContext, Future }

/**
 * The base of the migration type hierarchy.
 * Can contain any operation that can use an implicit `Session`.
 */
trait Migration {
  def run(db: JdbcBackend#Database)(implicit ec: ExecutionContext): Future[Unit]
  def statements: Seq[String] = Seq("-- No-SQL action --")
}

object Migration {
  implicit class MigrationConcat[M <: Migration](m: M) {
    /**
     *
     * @usecase def &(n: ReversibleMigration): ReversibleMigrationSeq
     * Append a [[ReversibleMigration]] to form either a
     * [[ReversibleMigrationSeq]] if the left side of `&` is also a [[ReversibleMigration]];
     * or else a plain [[MigrationSeq]]
     * @param n the [[ReversibleMigration]] to append
     * @example {{{ val combined = mig1 & mig2 & mig3 }}}
     *
     * @usecase def &(n: Migration): MigrationSeq
     * Append another [[Migration]] to form a [[MigrationSeq]]
     * @param n the [[Migration]] to append
     * @example {{{ val combined = mig1 & mig2 & mig3 }}}
     */
    def &[N <: Migration, O](n: N)(implicit ccm: CanConcatMigrations[M, N, O]): O = ccm.f(m, n)
  }

  def failure: Migration = new SqlMigration {
    override def actions: Seq[DBIO[_]] = Seq(FailureAction(new Exception("Aborted.")))
  }

  def successful: Migration = new SqlMigration {
    override val actions: Seq[DBIO[_]] = Seq(SuccessAction(()))
  }
}

/**
 * A [[Migration]] that can be reversed; that is,
 * it can provide a corresponding `Migration` that
 * will undo whatever this migration will do.
 */
trait ReversibleMigration extends Migration {
  def reverse: Migration
}

/**
 * A typeclass to determine the best way to combine migrations,
 * either into a [[ReversibleMigrationSeq]] or just a [[MigrationSeq]].
 * Used when you call '&' on [[Migration]]s.
 * Note that the migrations will be flattened; you will not end up with
 * something like `MigrationSeq(MigrationSeq(MigrationSeq(migA, migB), migC), migD)`.
 */
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

/**
 * Holds a sequence of [[Migration]]s and performs them one after the other.
 */
case class MigrationSeq(migrations: Migration*) extends Migration {

  final override def run(db: JdbcBackend#Database)(implicit ec: ExecutionContext): Future[Unit] =
    (migrations :+ Migration.successful).foldLeft(Future.successful(())){ case(f, m) =>
      f.flatMap(_ => m.run(db))
    }

  final override def statements: Seq[String] = migrations.flatMap(_.statements)
}

/**
 * Holds a sequence of [[ReversibleMigration]]s and performs them one after the other.
 */
class ReversibleMigrationSeq(override val migrations: ReversibleMigration*) extends MigrationSeq(migrations: _*) with ReversibleMigration {
  /**
   * @return the reverse [[MigrationSeq]]: Each migration will be reversed, and
   *         they will be in the reverse order.
   */
  def reverse = MigrationSeq(migrations.reverse.map(_.reverse): _*)
}

/**
 * A [[Migration]] defined in terms of SQL commands.
 * This trait implements `run` and instead defines an
 * abstract `actions` method.
 */
trait SqlMigration extends Migration {
  /**
   * The SQL statements to run
   */
  def actions: Seq[DBIO[_]]

  def seq: DBIO[Unit] = DBIO.seq(actions: _*)

  override def statements: Seq[String] = actions.collect {
    case sql: SqlAction[_, _, _] => sql.statements
    case _ => super.statements
  }.flatten

  /**
   * Runs all the SQL statements in a single transaction
   */
  override def run(db: JdbcBackend#Database)(implicit ec: ExecutionContext): Future[Unit] =
    db.run(DBIO.seq(actions: _*))
}

//TODO mechanism other than exceptions?
case class MigrationException(message: String, cause: Throwable) extends RuntimeException(message, cause)

/**
 * Convenience factory for [[SqlMigration]]
 * @example {{{ SqlMigration("drop table t1", "update t2 set x=10 where y=20") }}}
 */
object SqlMigration {
  def apply(actions: DBIO[_]*) = {
    def actions0 = actions
    new SqlMigration {
      override val actions: Seq[DBIO[_]] = actions0
    }
  }
}
