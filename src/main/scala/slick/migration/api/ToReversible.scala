package slick.migration.api

class ToReversible[-A <: Migration](val func: A => ReversibleMigration)

object ToReversible {
  implicit val reversible: ToReversible[ReversibleMigration] =
    new ToReversible[ReversibleMigration](identity)
}
