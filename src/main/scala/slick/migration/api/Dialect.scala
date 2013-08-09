package scala.slick
package migration.api

import driver._
import ast.{ FieldSymbol, TableNode }
import lifted.ForeignKeyAction

class HasDialect[D <: JdbcDriver](val f: D => Dialect[D])

object HasDialect {
  implicit def default[D <: JdbcDriver]: HasDialect[D] = new HasDialect(d => new Dialect(d))
  implicit object derby  extends HasDialect[DerbyDriver ](d => new DerbyDialect(d))
  implicit object h2     extends HasDialect[H2Driver    ](d => new H2Dialect(d))
  implicit object sqlite extends HasDialect[SQLiteDriver](d => new SQLiteDialect(d))
  implicit object hsqldb extends HasDialect[HsqldbDriver](d => new HsqldbDialect(d))
}

class Dialect[D <: JdbcDriver](driver: D) {
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

  def columnType(ci: ColumnInfo): String = ci.sqlType

  def autoInc(ci: ColumnInfo) = if(ci.autoInc) " AUTOINCREMENT" else ""

  def primaryKey(ci: ColumnInfo, newTable: Boolean) =
    (if (newTable && ci.isPk) " PRIMARY KEY" else "") + autoInc(ci)

  def notNull(ci: ColumnInfo) = if (ci.notNull) " NOT NULL" else ""

  def columnSql(ci: ColumnInfo, newTable: Boolean = true): String = {
    def name = quoteIdentifier(ci.name)
    def typ = columnType(ci)
    def default = ci.default.map(" DEFAULT " + _).getOrElse("")
    s"$name $typ$default${ notNull(ci) }${ primaryKey(ci, newTable) }"
  }

  def columnList(columns: Seq[FieldSymbol]) =
    quotedColumnNames(columns).mkString("(", ", ", ")")

  def createTable(table: TableNode, columns: Seq[ColumnInfo]): String =
    s"""create table ${quoteTableName(table)} (
      | ${columns map { columnSql(_, true) } mkString ", "}
      |)""".stripMargin

  def dropTable(table: TableNode): String =
    s"drop table ${quoteTableName(table)}"

  def renameTable(table: TableNode, to: String) =
    s"""alter table ${quoteTableName(table)}
      | rename to ${quoteIdentifier(to)}""".stripMargin

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

  def createIndex(index: IndexInfo) =
    s"""create ${if (index.unique) "unique" else ""}
      | index ${quoteIdentifier(index.name)} on ${quoteTableName(index.table)}
      | ${columnList(index.columns)}""".stripMargin

  def dropIndex(name: String) =
    s"drop index ${quoteIdentifier(name)}"

  def renameIndex(old: IndexInfo, newName: String): Seq[String] = List(
    s"alter index ${quoteIdentifier(old.name)} rename to ${quoteIdentifier(newName)}"
  )

  def addColumn(table: TableNode, column: ColumnInfo) =
    s"""alter table ${quoteTableName(table)}
      | add column ${columnSql(column, false)}""".stripMargin

  def dropColumn(table: TableNode, column: String) =
    s"""alter table ${quoteTableName(table)}
      | drop column ${quoteIdentifier(column)}""".stripMargin

  def renameColumn(table: TableNode, from: String, to: String) =
    s"""alter table ${quoteTableName(table)}
      | alter column ${quoteIdentifier(from)}
      | rename to ${quoteIdentifier(to)}""".stripMargin

  def alterColumnType(table: TableNode, column: ColumnInfo): Seq[String] = List(
    s"""alter table ${quoteTableName(table)}
      | alter column ${quoteIdentifier(column.name)}
      | set data type ${column.sqlType}""".stripMargin
  )

  def alterColumnDefault(table: TableNode, column: ColumnInfo) =
    s"""alter table ${quoteTableName(table)}
      | alter column ${quoteIdentifier(column.name)}
      | set default ${column.default getOrElse "null"}""".stripMargin

  def alterColumnNullability(table: TableNode, column: ColumnInfo) =
    s"""alter table ${quoteTableName(table)}
      | alter column ${quoteIdentifier(column.name)}
      | ${if (column.notNull) "set" else "drop"} not null""".stripMargin
}

class DerbyDialect(driver: DerbyDriver) extends Dialect[DerbyDriver](driver) {
  override def autoInc(ci: ColumnInfo) =
    if(ci.autoInc) " GENERATED BY DEFAULT AS IDENTITY" else ""

  override def alterColumnType(table: TableNode, column: ColumnInfo) = {
    val tmpColumnName = "tempcolumn"+(math.random*1000000).toInt
    val tmpColumn = column.copy(name = tmpColumnName)
    List(
      addColumn(table, tmpColumn),
      s"update ${quoteTableName(table)} set ${quoteIdentifier(tmpColumnName)} = ${quoteIdentifier(column.name)}",
      dropColumn(table, column.name),
      renameColumn(table, tmpColumnName, column.name)
    )
  }

  override def renameColumn(table: TableNode, from: String, to: String) =
    s"rename column ${quoteTableName(table)}.${quoteIdentifier(from)} to ${quoteIdentifier(to)}"

  override def alterColumnNullability(table: TableNode, column: ColumnInfo) =
    s"""alter table ${quoteTableName(table)}
      | alter column ${quoteIdentifier(column.name)}
      | ${if (column.notNull) "not" else ""} null""".stripMargin

  override def renameTable(table: TableNode, to: String) =
    s"rename table ${quoteTableName(table)} to ${quoteIdentifier(to)}"

  override def renameIndex(old: IndexInfo, newName: String) = List(
    s"rename index ${quoteIdentifier(old.name)} to ${quoteIdentifier(newName)}"
  )
}

class H2Dialect(driver: H2Driver) extends Dialect[H2Driver](driver) {
  override def columnType(ci: ColumnInfo): String =
    if (ci.autoInc) "SERIAL" else ci.sqlType
  override def autoInc(ci: ColumnInfo) = ""
}

class SQLiteDialect(driver: SQLiteDriver) extends Dialect[SQLiteDriver](driver) {
  override def columnType(ci: ColumnInfo): String =
    if (ci.autoInc) "INTEGER" else ci.sqlType
  override def renameIndex(old: IndexInfo, newName: String) =
    List(dropIndex(old.name), createIndex(old.copy(name = newName)))
}

class HsqldbDialect(driver: HsqldbDriver) extends Dialect[HsqldbDriver](driver) {
  override def autoInc(ci: ColumnInfo) =
    if(ci.autoInc) " GENERATED BY DEFAULT AS IDENTITY" else ""
  override def primaryKey(ci: ColumnInfo, newTable: Boolean) =
    autoInc(ci) + (if (newTable && ci.isPk) " PRIMARY KEY" else "")
  override def notNull(ci: ColumnInfo) =
    if (ci.notNull && !ci.isPk) " NOT NULL" else ""
}
