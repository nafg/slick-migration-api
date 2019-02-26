package slick.migration.api

import slick.jdbc.{JdbcProfile, JdbcType}
import slick.lifted.{Rep, TableQuery}


object AddColumnWithInitialValue {
  @deprecated("Use TableMigration#addColumnAndSetRaw instead", "0.6.0")
  def raw[T <: JdbcProfile#Table[_]](tableQuery: TableQuery[T])
                                    (col: T => Rep[_])
                                    (valueSql: String)
                                    (implicit dialect: Dialect[_]): Migration =
    TableMigration(tableQuery).addColumnAndSetRaw(col, valueSql)

  @deprecated("Use TableMigration#addColumnAndSet instead", "0.6.0")
  def apply[T <: JdbcProfile#Table[_], A](tableQuery: TableQuery[T])
                                         (col: T => Rep[A])
                                         (value: A)
                                         (implicit dialect: Dialect[_], jdbcType: JdbcType[A]) =
    TableMigration(tableQuery).addColumnAndSet(col, value)
}
