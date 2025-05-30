package slick.migration.api

/**
 * Holds a sequence of [[ReversibleMigration]]s and performs them one after the other.
 */
class ReversibleMigrationSeq(override val migrations: ReversibleMigration*)
  extends MigrationSeq(migrations *) with ReversibleMigration {
  /**
   * @return the reverse [[MigrationSeq]]: Each migration will be reversed, and
   *         they will be in the reverse order.
   */
  def reverse: MigrationSeq = MigrationSeq(migrations.reverse.map(_.reverse) *)
}
