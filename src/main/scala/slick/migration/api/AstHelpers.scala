package scala.slick
package migration.api

import scala.slick.ast.{ ColumnOption, FieldSymbol, Node, Select, TableNode }
import scala.slick.lifted.{ Column, Index }
import scala.slick.driver.JdbcDriver

case class TableInfo(schemaName: Option[String], tableName: String)

case class ColumnInfo(name: String, sqlType: String, notNull: Boolean, autoInc: Boolean, isPk: Boolean, default: Option[String])

case class IndexInfo(table: TableNode, name: String, unique: Boolean, columns: Seq[FieldSymbol])

trait AstHelpers {
  protected def tableInfo(table: TableNode): TableInfo = TableInfo(table.schemaName, table.tableName)

  protected def fieldSym(node: Node): Option[FieldSymbol] = node match {
    case Select(_, f: FieldSymbol) => Some(f)
    case _                         => None
  }

  protected def fieldSym(column: Column[_]): FieldSymbol =
    fieldSym(Node(column)) getOrElse sys.error("Invalid column: " + column)

  protected def columnInfo(driver: JdbcDriver, column: FieldSymbol): ColumnInfo = {
    val ti = driver.typeInfoFor(column.tpe)
    val initial = ColumnInfo(column.name, ti.sqlTypeName, !ti.nullable, false, false, None)
    column.options.foldLeft(initial) {
      case (ci, ColumnOption.DBType(s))  => ci.copy(sqlType = s)
      case (ci, ColumnOption.NotNull)    => ci.copy(notNull = true)
      case (ci, ColumnOption.Nullable)   => ci.copy(notNull = false)
      case (ci, ColumnOption.AutoInc)    => ci.copy(autoInc = true)
      case (ci, ColumnOption.PrimaryKey) => ci.copy(isPk = true)
      case (ci, ColumnOption.Default(v)) => ci.copy(default = Some(ti.valueToSQLLiteral(v)))
      case (ci, _)                       => ci
    }
  }

  protected def indexInfo(index: Index) = IndexInfo(index.table, index.name, index.unique, index.on flatMap (fieldSym(_)))
}
