package slick
package migration.api

import slick.jdbc._
import slick.ast.FieldSymbol
import slick.model.ForeignKeyAction

import AstHelpers._

/**
 * Base class for database dialects.
 * Provides methods that return the dialect-specific SQL strings
 * for performing various database operations.
 * The most important method is perhaps [[migrateTable]], which is called from
 * [[TableMigration#sql]].
 * These methods are to be overriden in database-specific subclasses as needed.
 * @tparam D The corresponding Slick profile type.
 *           Not used, but may come in handy in certain situations.
 */
class Dialect[-D <: JdbcProfile] extends AstHelpers {

  def quoteIdentifier(id: String): String = {
    val s = new StringBuilder(id.length + 4) append '"'
    for (c <- id) if (c == '"') s append "\"\"" else s append c
    (s append '"').toString
  }

  def quoteTableName(t: TableInfo): String = t.schemaName match {
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

  def createTable(table: TableInfo, columns: Seq[ColumnInfo]): String =
    s"""create table ${quoteTableName(table)} (
      | ${columns map { columnSql(_, newTable = true) } mkString ", "}
      |)""".stripMargin

  def dropTable(table: TableInfo): String =
    s"drop table ${quoteTableName(table)}"

  def renameTable(table: TableInfo, to: String) =
    s"""alter table ${quoteTableName(table)}
      | rename to ${quoteIdentifier(to)}""".stripMargin

  def createForeignKey(sourceTable: TableInfo, name: String, sourceColumns: Seq[FieldSymbol], targetTable: TableInfo, targetColumns: Seq[FieldSymbol], onUpdate: ForeignKeyAction, onDelete: ForeignKeyAction): String =
    s"""alter table ${quoteTableName(sourceTable)}
      | add constraint ${quoteIdentifier(name)}
      | foreign key ${columnList(sourceColumns)}
      | references ${quoteTableName(targetTable)}
      | (${quotedColumnNames(targetColumns) mkString ", "})
      | on update ${onUpdate.action} on delete ${onDelete.action}""".stripMargin

  def dropConstraint(table: TableInfo, name: String) =
    s"alter table ${quoteTableName(table)} drop constraint ${quoteIdentifier(name)}"

  def dropForeignKey(sourceTable: TableInfo, name: String) =
    dropConstraint(sourceTable, name)

  def createPrimaryKey(table: TableInfo, name: String, columns: Seq[FieldSymbol]) =
    s"""alter table ${quoteTableName(table)}
      | add constraint ${quoteIdentifier(name)} primary key
      | ${columnList(columns)}""".stripMargin

  def dropPrimaryKey(table: TableInfo, name: String) =
    dropConstraint(table, name)

  def createIndex(index: IndexInfo) =
    s"""create ${if (index.unique) "unique" else ""}
      | index ${quoteIdentifier(index.name)} on ${quoteTableName(tableInfo(index.table))}
      | ${columnList(index.columns)}""".stripMargin

  def dropIndex(index: IndexInfo) =
    s"drop index ${quoteIdentifier(index.name)}"

  def renameIndex(old: IndexInfo, newName: String): Seq[String] = List(
    s"alter index ${quoteIdentifier(old.name)} rename to ${quoteIdentifier(newName)}"
  )

  def addColumn(table: TableInfo, column: ColumnInfo) =
    s"""alter table ${quoteTableName(table)}
      | add column ${columnSql(column, newTable = false)}""".stripMargin

  def dropColumn(table: TableInfo, column: String) =
    s"""alter table ${quoteTableName(table)}
      | drop column ${quoteIdentifier(column)}""".stripMargin

  def renameColumn(table: TableInfo, from: ColumnInfo, to: String) =
    s"""alter table ${quoteTableName(table)}
      | alter column ${quoteIdentifier(from.name)}
      | rename to ${quoteIdentifier(to)}""".stripMargin

  def alterColumnType(table: TableInfo, column: ColumnInfo): Seq[String] = List(
    s"""alter table ${quoteTableName(table)}
      | alter column ${quoteIdentifier(column.name)}
      | set data type ${column.sqlType}""".stripMargin
  )

  def alterColumnDefault(table: TableInfo, column: ColumnInfo) =
    s"""alter table ${quoteTableName(table)}
      | alter column ${quoteIdentifier(column.name)}
      | set default ${column.default getOrElse "null"}""".stripMargin

  def alterColumnNullability(table: TableInfo, column: ColumnInfo) =
    s"""alter table ${quoteTableName(table)}
      | alter column ${quoteIdentifier(column.name)}
      | ${if (column.notNull) "set" else "drop"} not null""".stripMargin

  /*
   *  DROP | CREATE | RENAME | ACTION
   *  false| false  | None   | assume table exists and do things via alter table
   *  false| false  | Some   | rename table, plus as above
   *  false| true   | None   | put as much as possible in the create table statement
   *  false| true   | Some   | create with new name, plus as above
   *  true | false  | None   | just drop it
   *  true | false  | Some   | let it error (type safety??)
   *  true | true   | None   | drop, then create table with as much as possible
   *  true | true   | Some   | drop, then create table with new name, with as much as possible
   *
   *
   * In summary:
   *  - if drop, do the drop then continue
   *  - if !drop, continue
   *  - if create, incorporate as much as possible into the create statement, including any rename
   *  - if !create, do rename + other alters
   */
  def migrateTable[E <: D](table: TableInfo, tmd: TableMigrationData): Seq[String] = {
    val drop =
      if(!tmd.tableDrop) Nil
      else Seq(dropTable(table))

    val createColumns = if(tmd.tableCreate) {
      val tbl = tmd.tableRename match {
        case Some(name) => table.copy(tableName = name)
        case None       => table
      }
      Seq(createTable(tbl, tmd.columnsCreate))
    } else
      tmd.tableRename.map{ renameTable(table, _) }.toList ++
      tmd.columnsCreate.map{ addColumn(table, _) }
    val modifyColumns =
      tmd.columnsDrop.map{ c => dropColumn(table, c.name) } ++
      tmd.columnsRename.map{ case (k, v) => renameColumn(table, k, v) } ++
      tmd.columnsAlterType.flatMap{ alterColumnType(table, _) } ++
      tmd.columnsAlterDefault.map{ alterColumnDefault(table, _) } ++
      tmd.columnsAlterNullability.map{ alterColumnNullability(table, _) }
    val migrateIndexes =
      tmd.primaryKeysDrop.map{ pk => dropPrimaryKey(table, pk._1) } ++
      tmd.primaryKeysCreate.map{ case (name, cols) => createPrimaryKey(table, name, cols) } ++
      tmd.foreignKeysDrop.map{ fk => dropForeignKey(table, fk.name) } ++
      tmd.foreignKeysCreate.map{ fk =>
        createForeignKey(
          table,
          fk.name,
          fk.linearizedSourceColumns.flatMap(fieldSym(_).toSeq),
          tableInfo(fk.targetTable),
          fk.linearizedTargetColumnsForOriginalTargetTable.flatMap(fieldSym(_).toSeq),
          fk.onUpdate,
          fk.onDelete
        )
      } ++
      tmd.indexesDrop.map{ dropIndex } ++
      tmd.indexesCreate.map{ createIndex } ++
      tmd.indexesRename.flatMap{ case (k, v) => renameIndex(k, v) }

    drop ++
      createColumns ++
      modifyColumns ++
      migrateIndexes
  }
}

class DerbyDialect extends Dialect[DerbyProfile] {
  override def autoInc(ci: ColumnInfo) =
    if(ci.autoInc) " GENERATED BY DEFAULT AS IDENTITY" else ""

  override def alterColumnType(table: TableInfo, column: ColumnInfo) = {
    val tmpColumnName = "tempcolumn"+(math.random*1000000).toInt
    val tmpColumn = column.copy(name = tmpColumnName)
    List(
      addColumn(table, tmpColumn),
      s"update ${quoteTableName(table)} set ${quoteIdentifier(tmpColumnName)} = ${quoteIdentifier(column.name)}",
      dropColumn(table, column.name),
      renameColumn(table, tmpColumn, column.name)
    )
  }

  override def renameColumn(table: TableInfo, from: ColumnInfo, to: String) =
    s"rename column ${quoteTableName(table)}.${quoteIdentifier(from.name)} to ${quoteIdentifier(to)}"

  override def alterColumnNullability(table: TableInfo, column: ColumnInfo) =
    s"""alter table ${quoteTableName(table)}
      | alter column ${quoteIdentifier(column.name)}
      | ${if (column.notNull) "not" else ""} null""".stripMargin

  override def renameTable(table: TableInfo, to: String) =
    s"rename table ${quoteTableName(table)} to ${quoteIdentifier(to)}"

  override def renameIndex(old: IndexInfo, newName: String) = List(
    s"rename index ${quoteIdentifier(old.name)} to ${quoteIdentifier(newName)}"
  )
}

class H2Dialect extends Dialect[H2Profile] {
  override def columnType(ci: ColumnInfo): String =
    if (ci.autoInc) "SERIAL" else ci.sqlType
  override def autoInc(ci: ColumnInfo) = ""
}

trait SimulatedRenameIndex[T <: JdbcProfile] { this: Dialect[T] =>
  override def renameIndex(old: IndexInfo, newName: String): Seq[String] =
    List(dropIndex(old), createIndex(old.copy(name = newName)))
}

class SQLiteDialect extends Dialect[SQLiteProfile] with SimulatedRenameIndex[SQLiteProfile] {
  override def columnType(ci: ColumnInfo): String =
    if (ci.autoInc) "INTEGER" else ci.sqlType
}

class HsqldbDialect extends Dialect[HsqldbProfile] {
  override def autoInc(ci: ColumnInfo) =
    if(ci.autoInc) " GENERATED BY DEFAULT AS IDENTITY" else ""
  override def primaryKey(ci: ColumnInfo, newTable: Boolean) =
    autoInc(ci) + (if (newTable && ci.isPk) " PRIMARY KEY" else "")
  override def notNull(ci: ColumnInfo) =
    if (ci.notNull && !ci.isPk) " NOT NULL" else ""
}

class MySQLDialect extends Dialect[MySQLProfile] with SimulatedRenameIndex[MySQLProfile] {
  override def autoInc(ci: ColumnInfo) = if(ci.autoInc) " AUTO_INCREMENT" else ""

  override def quoteIdentifier(id: String): String = {
    val s = new StringBuilder(id.length + 4) append '`'
    for (c <- id) if (c == '"') s append "\"\"" else s append c
    (s append '`').toString
  }

  override def dropIndex(index: IndexInfo) =
    s"drop index ${quoteIdentifier(index.name)} on ${quoteTableName(tableInfo(index.table))}"

  override def renameColumn(table: TableInfo, from: ColumnInfo, to: String) = {
    val newCol = from.copy(name = to)
    s"""alter table ${quoteTableName(table)}
      | change ${quoteIdentifier(from.name)}
      | ${columnSql(newCol, newTable = false)}""".stripMargin
  }

  override def alterColumnNullability(table: TableInfo, column: ColumnInfo) =
    renameColumn(table, column, column.name)

  override def alterColumnType(table: TableInfo, column: ColumnInfo) =
    Seq(renameColumn(table, column, column.name))

  override def dropForeignKey(table: TableInfo, name: String) =
    s"alter table ${quoteTableName(table)} drop foreign key ${quoteIdentifier(name)}"

  override def createPrimaryKey(table: TableInfo, name: String, columns: Seq[FieldSymbol]) =
    s"""alter table ${quoteTableName(table)}
      | add constraint primary key
      | ${columnList(columns)}""".stripMargin
  override def dropPrimaryKey(table: TableInfo, name: String) =
    s"alter table ${quoteTableName(table)} drop primary key"
}

class PostgresDialect extends Dialect[PostgresProfile] {
  override def columnType(ci: ColumnInfo): String =
    if (ci.autoInc) "SERIAL" else ci.sqlType
  override def autoInc(ci: ColumnInfo) = ""
  override def renameColumn(table: TableInfo, from: ColumnInfo, to: String) =
    s"""alter table ${quoteTableName(table)}
      | rename column ${quoteIdentifier(from.name)}
      | to ${quoteIdentifier(to)}""".stripMargin
}

object GenericDialect {

  def apply(profile: JdbcProfile): Dialect[_ <: JdbcProfile] = profile match {
    case DerbyProfile    => new DerbyDialect
    case H2Profile       => new H2Dialect
    case SQLiteProfile   => new SQLiteDialect
    case HsqldbProfile   => new HsqldbDialect
    case MySQLProfile    => new MySQLDialect
    case PostgresProfile => new PostgresDialect
    case _ =>
      throw new IllegalArgumentException("Slick error : Unknown or unsupported jdbc profile found.")
  }
}
