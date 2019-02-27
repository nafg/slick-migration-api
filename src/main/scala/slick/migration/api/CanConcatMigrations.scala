package slick
package migration.api


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
  implicit def reversible[A <: Migration, B <: Migration](implicit reverseA: ToReversible[A],
                                                          reverseB: ToReversible[B]): CanConcatMigrations[A, B, ReversibleMigrationSeq] =
    new CanConcatMigrations({
      case (rms: ReversibleMigrationSeq, b) => new ReversibleMigrationSeq(rms.migrations :+ reverseB.func(b): _*)
      case (a, b)                           => new ReversibleMigrationSeq(reverseA.func(a), reverseB.func(b))
    })
}
