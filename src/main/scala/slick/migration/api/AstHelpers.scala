package slick
package migration
package api

import slick.ast.{ FieldSymbol, Node, Select, TableNode }
import slick.driver.JdbcDriver
import slick.lifted.{ Rep, Index }
import slick.profile.{ RelationalProfile, SqlProfile }

private [api] object AstHelpers {

  /**
    * Internal lightweight data structure, containing
    * information for schema manipulation about a table per se
    *
    * @param schemaName the name of the database schema (namespace) the table is in, if any
    * @param tableName  the name of the table itself
    */
  case class TableInfo(schemaName: Option[String], tableName: String)

  /**
    * Internal lightweight data structure, containing
    * information for schema manipulation about a column
    *
    * @param name    the column name
    * @param sqlType the column's data type, in the database's SQL dialect
    * @param notNull `true` for `NOT NULL`, `false` for `NULL` (nullable column).
    * @param autoInc `true` if the column is AUTOINCREMENT or equivalent. Corresponds to `O.AutoInc`
    * @param isPk    `true` if the column should be declared a primary key inline with the column (in the column list).
    *                Corresponds to `O.PrimaryKey`
    * @param default An `Option`al default value, in the database's SQL syntax.
                  Corresponds to `O.Default`
    */
  case class ColumnInfo(name: String, sqlType: String, notNull: Boolean, autoInc: Boolean, isPk: Boolean, default: Option[String])

  /**
    * Internal lightweight data structure, containing
    * information for schema manipulation about an index
    *
    * @param table   The Slick table object
    * @param name    The name of the index
    * @param unique  Whether the column can contain duplicates
    * @param columns The columns that this index applies to, as `scala.slick.ast.FieldSymbol`
    */
  case class IndexInfo(table: TableNode, name: String, unique: Boolean, columns: Seq[FieldSymbol])
}

/**
 * Helper trait for converting various representations of tables, columns, and indexes
 */
private [api] trait AstHelpers {

  import AstHelpers._

  /**
   * @param table a Slick table object whose qualified name is needed
   * @return a `TableInfo` representing the qualified name of `table`
   */
  protected def tableInfo(table: TableNode): TableInfo = TableInfo(table.schemaName, table.tableName)

  /**
   * @return if `node` represents a reference to a table's column, that is, it is a `Select(_, f: FieldSymbol)`,
   *         then `Some(f)`; otherwise `None`
   */
  protected def fieldSym(node: Node): Option[FieldSymbol] = node match {
    case Select(_, f: FieldSymbol) => Some(f)
    case _                         => None
  }

  /**
   * @return a `FieldSymbol` representing the column
   */
  protected def fieldSym(column: Rep[_]): FieldSymbol =
    fieldSym(column.toNode) getOrElse sys.error("Invalid column: " + column)

  /**
   * @param driver a Slick driver, used to extract `ColumnInfo#sqlType` and `ColumnInfo#notNull`
   *               by calling `typeInfoFor`
   * @return a `ColumnInfo` representing the relevant information in `column`
   */
  protected def columnInfo(driver: JdbcDriver, column: FieldSymbol): ColumnInfo = {

    import ast.{ ColumnOption => AColumnOption }
    import RelationalProfile.{ ColumnOption => RColumnOption }
    import SqlProfile.{ ColumnOption => SColumnOption }

    column.tpe match {
      case driver.JdbcType(ti, isOpt) =>

        val initial = ColumnInfo(
          name = column.name,
          sqlType = ti.sqlTypeName(column.options.collectFirst{case len: RColumnOption.Length => len}),
          notNull = !(isOpt || ti.scalaType.nullable),
          autoInc = false,
          isPk = false,
          default = None
        )

        column.options.foldLeft(initial) {
          case (ci, SColumnOption.SqlType(s)) => ci.copy(sqlType = s)
          case (ci, SColumnOption.NotNull)    => ci.copy(notNull = true)
          case (ci, SColumnOption.Nullable)   => ci.copy(notNull = false)
          case (ci, AColumnOption.AutoInc)    => ci.copy(autoInc = true)
          case (ci, AColumnOption.PrimaryKey) => ci.copy(isPk = true)
          case (ci, RColumnOption.Default(v)) => ci.copy(default = Some(ti.valueToSQLLiteral(v)))
          case (ci, _)                        => ci
        }
    }
  }

  /**
   * @return an `IndexInfo` containing the relevant information from a Slick `Index`
   */
  protected def indexInfo(index: Index) = {
    IndexInfo(
      table = index.table.tableNode,
      name = index.name,
      unique = index.unique,
      columns = index.on.flatMap(node => fieldSym(node))
    )
  }
}
