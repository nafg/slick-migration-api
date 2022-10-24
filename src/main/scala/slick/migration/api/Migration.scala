package slick.migration.api

import slick.dbio.DBIO


/**
 * The base of the migration type hierarchy.
 * Produces a DBIO that runs the migration
 */
trait Migration {
  def apply(): DBIO[Unit]
}

object Migration {
  implicit class MigrationConcat[M <: Migration](m: M) {
    /**
     * Append another [[Migration]] to form a [[MigrationSeq]]
     * If both sides are [[ReversibleMigration]]s then a [[ReversibleMigrationSeq]]
     * is returned.
     * @param n the [[Migration]] to append
     * @example {{{ val combined = mig1 & mig2 & mig3 }}}
     */
    def &[N <: Migration, O](n: N)(implicit ccm: CanConcatMigrations[M, N, O]): O = ccm.f(m, n)
  }

  def empty: Migration = new Migration {
    override def apply() = DBIO.successful(())
  }

  def apply(action: => DBIO[Unit]): Migration = new Migration {
    override def apply() = action
  }

}
