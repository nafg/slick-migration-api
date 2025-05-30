package slick.migration.api

import slick.jdbc.{JdbcProfile, JdbcType}
import slick.lifted.{Rep, TableQuery}


object AddColumnWithInitialValue {
  @deprecated("Use TableMigration#addColumnAndSetRaw instead", "0.6.0")
  def raw[T <: JdbcProfile#Table[?]](tableQuery: TableQuery[T])
                                    (col: T => Rep[?])
                                    (valueSql: String)
                                    (implicit dialect: Dialect[?]): Migration =
    TableMigration(tableQuery).addColumnAndSetRaw(col, valueSql)

  @deprecated("Use TableMigration#addColumnAndSet instead", "0.6.0")
  def apply[T <: JdbcProfile#Table[?], A](tableQuery: TableQuery[T])
                                         (col: T => Rep[A])
                                         (value: A)
                                         (implicit dialect: Dialect[?], jdbcType: JdbcType[A]) =
    TableMigration(tableQuery).addColumnAndSet(col, value)
}
