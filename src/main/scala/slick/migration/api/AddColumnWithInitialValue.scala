package slick.migration.api

import slick.jdbc.{JdbcProfile, JdbcType}
import slick.lifted.{Rep, TableQuery}
import slick.migration.api.AstHelpers.TableInfo


object AddColumnWithInitialValue extends AstHelpers {
  def raw[T <: JdbcProfile#Table[_]](tableQuery: TableQuery[T])
                                    (col: T => Rep[_])
                                    (valueSql: String)
                                    (implicit dialect: Dialect[_]) = {
    val table = tableQuery.baseTableRow

    val tableInfo = TableInfo(table.schemaName, table.tableName)

    val columnInfo = colInfo(table)(col)

    SqlMigration(dialect.addColumn(tableInfo, columnInfo.copy(notNull = false))) &
      SqlMigration(
        s"UPDATE ${dialect.quoteTableName(tableInfo)} SET ${dialect.quoteIdentifier(columnInfo.name)} = $valueSql;"
      ) &
      SqlMigration(dialect.alterColumnNullability(tableInfo, columnInfo))
  }

  def apply[T <: JdbcProfile#Table[_], A](tableQuery: TableQuery[T])
                                         (col: T => Rep[A])
                                         (value: A)
                                         (implicit dialect: Dialect[_],
                                          jdbcType: JdbcType[A]) =
    raw[T](tableQuery)(col)(jdbcType.valueToSQLLiteral(value))(dialect)
}
