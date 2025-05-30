package slick.migration.api

import slick.dbio.DBIO


/**
 * Holds a sequence of [[Migration]]s and performs them one after the other.
 */
case class MigrationSeq(migrations: Migration*) extends Migration {
  final def apply() = DBIO.seq(migrations.map(_()) *)
}
