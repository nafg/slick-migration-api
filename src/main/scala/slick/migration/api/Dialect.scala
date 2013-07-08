package scala.slick
package migration.api

import driver.JdbcDriver
import ast.{ FieldSymbol, TableNode }
import lifted.ForeignKeyAction

class HasDialect[D <: JdbcDriver](val f: D => Dialect[D])

object HasDialect {
  implicit def default[D <: JdbcDriver]: HasDialect[D] = new HasDialect(d => new Dialect(d))
}

class Dialect[D <: JdbcDriver](driver: D) {
  import driver._

  def quoteIdentifier(id: String): String = {
    val s = new StringBuilder(id.length + 4) append '"'
    for (c <- id) if (c == '"') s append "\"\"" else s append c
    (s append '"').toString
  }

  def quoteTableName(t: TableNode): String = t.schemaName match {
    case Some(s) => quoteIdentifier(s) + "." + quoteIdentifier(t.tableName)
    case None    => quoteIdentifier(t.tableName)
  }

  protected def quotedColumnNames(ns: Seq[FieldSymbol]) = ns.map(fs => quoteIdentifier(fs.name))

  def columnSql(ci: ColumnInfo, includePk: Boolean = true): String = {
    def name = quoteIdentifier(ci.name)
    def typ = if (ci.autoInc) "SERIAL" else ci.sqlType
    def default = ci.default.map(" DEFAULT " + _).getOrElse("")
    def notNull = if (ci.notNull) " NOT NULL" else ""
    def pk = if (includePk && ci.isPk) " PRIMARY KEY" else ""
    s"$name $typ$default$notNull$pk"
  }

  def columnList(columns: Seq[FieldSymbol]) =
    quotedColumnNames(columns).mkString("(", ", ", ")")

  def createTable(table: TableNode, columns: Seq[ColumnInfo]): String =
    s"""create table ${quoteTableName(table)} (
      | ${columns map { columnSql(_, true) } mkString ", "}
      |)""".stripMargin

  def dropTable(table: TableNode): String =
    s"drop table ${quoteTableName(table)}"

  def createForeignKey(sourceTable: TableNode, name: String, sourceColumns: Seq[FieldSymbol], targetTable: TableNode, targetColumns: Seq[FieldSymbol], onUpdate: ForeignKeyAction, onDelete: ForeignKeyAction): String =
    s"""alter table ${quoteTableName(sourceTable)}
      | add constraint ${quoteIdentifier(name)}
      | foreign key ${columnList(sourceColumns)}
      | references ${quoteTableName(targetTable)}
      | (${quotedColumnNames(targetColumns) mkString ", "})
      | on update ${onUpdate.action} on delete ${onDelete.action}""".stripMargin

  def dropConstraint(table: TableNode, name: String) =
    s"alter table ${quoteTableName(table)} drop constraint ${quoteIdentifier(name)}"

  def dropForeignKey(sourceTable: TableNode, name: String) =
    dropConstraint(sourceTable, name)

  def createPrimaryKey(table: TableNode, name: String, columns: Seq[FieldSymbol]) =
    s"""alter table ${quoteTableName(table)}
      | add constraint ${quoteIdentifier(name)} primary key
      | ${columnList(columns)}""".stripMargin

  def dropPrimaryKey(table: TableNode, name: String) =
    dropConstraint(table, name)

  def createIndex(table: TableNode, name: String, unique: Boolean, columns: Seq[FieldSymbol]) =
    s"""create ${if (unique) "unique" else ""}
      | index ${quoteIdentifier(name)} on ${quoteTableName(table)}
      | ${columnList(columns)}""".stripMargin

  def dropIndex(name: String) =
    s"drop index ${quoteIdentifier(name)}"

  def alterColumnType(table: TableNode, column: ColumnInfo) =
    s"""alter table ${quoteTableName(table)}
      | alter column ${quoteIdentifier(column.name)}
      | type ${column.sqlType}""".stripMargin

  def alterColumnDefault(table: TableNode, column: ColumnInfo) =
    s"""alter table ${quoteTableName(table)}
      | alter column ${quoteIdentifier(column.name)}
      | set default ${column.default getOrElse "null"}""".stripMargin

  def alterColumnNullability(table: TableNode, column: ColumnInfo) =
    s"""alter table ${quoteTableName(table)}
      | alter column ${quoteIdentifier(column.name)}
      | ${if (column.notNull) "set" else "drop"} not null""".stripMargin
}
