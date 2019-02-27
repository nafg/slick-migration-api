package slick.migration.api

import slick.jdbc.SimpleJdbcAction
import slick.sql


/**
 * A [[Migration]] defined in terms of SQL commands.
 * This trait implements `apply` and instead defines an
 * abstract [[sql]] method.
 */
trait SqlMigration extends Migration {
  /**
   * The SQL statements to run
   */
  def sql: Seq[String]

  def apply() = SimpleJdbcAction { ctx =>
    for (str <- sql)
      ctx.session.withPreparedStatement(str)(_.execute())
  }
}

/**
 * Convenience factory for [[SqlMigration]]
 *
 * @example {{{ SqlMigration("drop table t1", "update t2 set x=10 where y=20") }}}
 */
object SqlMigration {
  def apply(sql: String*): SqlMigration = {
    def sql0 = sql

    new SqlMigration {
      override val sql = sql0
    }
  }
}
