package slick
package migration.api

import slick.ast.FieldSymbol
import slick.jdbc._
import slick.migration.api.AstHelpers._
import slick.migration.api.TableMigration.Action._
import slick.model.ForeignKeyAction

import java.security.SecureRandom

/**
 * Base class for database dialects.
 * Provides methods that return the dialect-specific SQL strings
 * for performing various database operations.
 * The most important method is perhaps [[migrateTable]], which is called from
 * [[TableMigration#sql]].
 * These methods are to be overridden in database-specific subclasses as needed.
 * @tparam P The corresponding Slick driver type.
 *           Not used, but may come in handy in certain situations.
 */
class Dialect[-P <: JdbcProfile] extends AstHelpers {

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

  def columnSql(ci: ColumnInfo, newTable: Boolean): String = {
    def name = quoteIdentifier(ci.name)
    def typ = columnType(ci)
    def default = ci.default.map(" DEFAULT " + _).getOrElse("")
    s"$name $typ$default${ notNull(ci) }${ primaryKey(ci, newTable) }"
  }

  def columnList(columns: Seq[FieldSymbol]) =
    quotedColumnNames(columns).mkString("(", ", ", ")")

  def createTable(table: TableInfo, columns: Seq[ColumnInfo]): List[String] = List(
    s"""create table ${quoteTableName(table)} (
      | ${columns map { columnSql(_, newTable = true) } mkString ", "}
      |)""".stripMargin
  )

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

  def renameIndex(old: IndexInfo, newName: String): List[String] = List(
    s"alter index ${quoteIdentifier(old.name)} rename to ${quoteIdentifier(newName)}"
  )

  def addColumn(table: TableInfo, column: ColumnInfo) =
    s"""alter table ${quoteTableName(table)}
      | add column ${columnSql(column, newTable = false)}""".stripMargin

  def addColumnWithInitialValue(table: TableInfo, column: ColumnInfo, rawSqlExpr: String) =
    List(addColumn(table, column.copy(default = Some(rawSqlExpr)))) ++
      (if (column.default.contains(rawSqlExpr)) Nil else List(alterColumnDefault(table, column)))

  def dropColumn(table: TableInfo, column: String): List[String] = List(
    s"""alter table ${quoteTableName(table)}
      | drop column ${quoteIdentifier(column)}""".stripMargin
  )

  def renameColumn(table: TableInfo, from: String, to: String) =
    s"""alter table ${quoteTableName(table)}
       | alter column ${quoteIdentifier(from)}
       | rename to ${quoteIdentifier(to)}""".stripMargin

  def renameColumn(table: TableInfo, from: ColumnInfo, to: String): String = renameColumn(table, from.name, to)

  def alterColumnType(table: TableInfo, column: ColumnInfo): List[String] = List(
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

  private def partition[A, B](xs: List[A])(toB: PartialFunction[A, B]): (List[B], List[A]) =
    xs.foldLeft((List.empty[B], List.empty[A])) {
      case ((bs, as), a) =>
        toB.andThen(b => (b :: bs, as)).applyOrElse(a, (_: A) => (bs, a :: as))
    }

  def migrateTable(table: TableInfo, actions: List[TableMigration.Action]): List[String] = {
    def loop(actions: List[TableMigration.Action]): List[String] = actions match {
      case Nil                                             => Nil
      case CreateTable :: rest                             =>
        val (cols, other) = partition(rest) { case a: AddColumn => a }
        createTable(table, cols.map(_.info)) ::: loop(other)
      case AlterColumnType(info) :: rest                   => alterColumnType(table, info) ::: loop(rest)
      case DropTable :: rest                               => dropTable(table) :: loop(rest)
      case RenameTableTo(to) :: rest                       => renameTable(table, to) :: loop(rest)
      case RenameTableFrom(from) :: rest                   => renameTable(table.copy(tableName = from), table.tableName) :: loop(rest)
      case AddColumn(info) :: rest                         => addColumn(table, info) :: loop(rest)
      case AddColumnAndSetInitialValue(info, expr) :: rest => addColumnWithInitialValue(table, info, expr) ::: loop(rest)
      case DropColumn(info) :: rest                        =>
        (if (rest contains DropTable) Nil else dropColumn(table, info.name)) ::: loop(rest)
      case DropColumnOfName(name) :: rest                  =>
        (if (rest contains DropTable) Nil else dropColumn(table, name)) ::: loop(rest)
      case RenameColumnTo(originalInfo, to) :: rest        => renameColumn(table, originalInfo, to) :: loop(rest)
      case RenameColumnFrom(currentInfo, from) :: rest     => renameColumn(table, from, currentInfo.name) :: loop(rest)
      case AlterColumnDefault(info) :: rest                => alterColumnDefault(table, info) :: loop(rest)
      case AlterColumnNullable(info) :: rest               => alterColumnNullability(table, info) :: loop(rest)
      case DropPrimaryKey(info) :: rest                    => dropPrimaryKey(table, info.name) :: loop(rest)
      case AddPrimaryKey(info) :: rest                     => createPrimaryKey(table, info.name, info.columns) :: loop(rest)
      case DropForeignKey(fk) :: rest                      => dropForeignKey(table, fk.name) :: loop(rest)
      case AddForeignKey(fk) :: rest                       =>
        val sql =
          createForeignKey(
            sourceTable = table,
            name = fk.name,
            sourceColumns = fk.linearizedSourceColumns.flatMap(fieldSym(_).toSeq),
            targetTable = tableInfo(fk.targetTable),
            targetColumns = fk.linearizedTargetColumnsForOriginalTargetTable.flatMap(fieldSym(_).toSeq),
            onUpdate = fk.onUpdate,
            onDelete = fk.onDelete
          )
        sql :: loop(rest)
      case DropIndex(info) :: rest                         => dropIndex(info) :: loop(rest)
      case CreateIndex(info) :: rest                       => createIndex(info) :: loop(rest)
      case RenameIndexTo(originalInfo, to) :: rest         => renameIndex(originalInfo, to) ::: loop(rest)
      case RenameIndexFrom(currentInfo, from) :: rest      => renameIndex(currentInfo.copy(name = from), currentInfo.name) ::: loop(rest)
    }

    loop(actions.reverse.sortBy(_.sort))
  }
}

class DerbyDialect extends Dialect[DerbyProfile] {
  override def autoInc(ci: ColumnInfo) =
    if(ci.autoInc) " GENERATED BY DEFAULT AS IDENTITY" else ""

  override def alterColumnType(table: TableInfo, column: ColumnInfo) = {
    val tmpColumnName = "temp_column"+(math.random*1000000).toInt
    val tmpColumn = column.copy(name = tmpColumnName)

    addColumn(table, tmpColumn) ::
    s"update ${quoteTableName(table)} set ${quoteIdentifier(tmpColumnName)} = ${quoteIdentifier(column.name)}" ::
    dropColumn(table, column.name) :::
    renameColumn(table, tmpColumn.name, column.name) :: Nil
  }

  override def renameColumn(table: TableInfo, from: String, to: String) =
    s"rename column ${quoteTableName(table)}.${quoteIdentifier(from)} to ${quoteIdentifier(to)}"

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
  override def renameIndex(old: IndexInfo, newName: String): List[String] =
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

  /**
   * Requires MySQL 8.0
   */
  override def renameColumn(table: TableInfo, from: String, to: String) =
    s"ALTER TABLE ${quoteTableName(table)} RENAME COLUMN ${quoteIdentifier(from)} TO ${quoteIdentifier(to)}"

  override def renameColumn(table: TableInfo, from: ColumnInfo, to: String) = {
    val newCol = from.copy(name = to)
    s"""alter table ${quoteTableName(table)}
      | change ${quoteIdentifier(from.name)}
      | ${columnSql(newCol, newTable = false)}""".stripMargin
  }

  override def alterColumnNullability(table: TableInfo, column: ColumnInfo) =
    renameColumn(table, column, column.name)

  override def alterColumnType(table: TableInfo, column: ColumnInfo) =
    List(renameColumn(table, column, column.name))

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
    (ci.autoInc, ci.sqlType) match {
      case (false, t)         => t
      case (true, "SMALLINT") => "SMALLSERIAL"
      case (true, "INTEGER")  => "SERIAL"
      case (true, "BIGINT")   => "BIGSERIAL"
      case (true, _)          => throw new RuntimeException("Unsupported autoincrement type")
    }
  override def autoInc(ci: ColumnInfo) = ""
  override def renameColumn(table: TableInfo, from: String, to: String) =
    s"""alter table ${quoteTableName(table)}
       | rename column ${quoteIdentifier(from)}
       | to ${quoteIdentifier(to)}""".stripMargin
}

class OracleDialect extends Dialect[OracleProfile] {

  override def createTable(table: TableInfo, columns: Seq[ColumnInfo]): List[String] = {
    super.createTable(table, columns) ++
      columns.filter(_.autoInc).flatMap(addAutoInc(table, _, 1L))
  }

  override def quoteIdentifier(id: String): String = {
    super.quoteIdentifier(id).toUpperCase
  }

  private def autoIncNames(t: TableInfo, c: ColumnInfo) = {
    val random = new scala.util.Random(new SecureRandom())
    (s"SEQ_${random.alphanumeric.take(16).mkString}", s"TRG_${random.alphanumeric.take(16).mkString}")
  }

  private def addAutoInc(table: TableInfo, column: ColumnInfo, startWith: Long): List[String] = {
    val (seq, trg) = autoIncNames(table, column)
    val tab = quoteTableName(table)
    val col = quoteIdentifier(column.name)

    List(
      s"CREATE SEQUENCE $seq START WITH $startWith INCREMENT BY 1",
      s"""
        | DECLARE
        | stmt VARCHAR(1000);
        | BEGIN
        |   stmt := 'CREATE OR REPLACE TRIGGER $trg
        |   BEFORE INSERT
        |   ON $tab
        |   REFERENCING NEW AS NEW
        |   FOR EACH ROW WHEN (NEW.$col IS NULL)
        |   BEGIN SELECT $seq.nextval INTO :NEW.$col FROM sys.dual;
        |   END;';
        |   EXECUTE IMMEDIATE stmt;
        | END;""".stripMargin
    )
  }

  private def dropTriggerAndSequence(table: TableInfo, name: String) = {
      val searchTrigger = s"""SELECT USER_TRIGGER_COLS.TRIGGER_NAME AS TRG_NAME, USER_DEPENDENCIES.referenced_name AS SEQ_NAME
                              | FROM USER_TRIGGER_COLS
                              | INNER JOIN USER_DEPENDENCIES on USER_TRIGGER_COLS.TRIGGER_NAME = USER_DEPENDENCIES.Name
                              | WHERE USER_DEPENDENCIES.referenced_type = 'SEQUENCE' AND USER_DEPENDENCIES.type = 'TRIGGER'
                              | AND USER_TRIGGER_COLS.COLUMN_NAME = '${name.toUpperCase}'
                              | AND USER_TRIGGER_COLS.TABLE_NAME= '${table.tableName.toUpperCase}'""".stripMargin

      s"""DECLARE
          |  trgName varchar2(30);
          |  seqName varchar2(30);
          | BEGIN
          |  FOR results IN ($searchTrigger) LOOP
          |    trgName := results.TRG_NAME;
          |    seqName := results.SEQ_NAME;
          |    EXECUTE IMMEDIATE 'DROP TRIGGER ' || trgName;
          |    EXECUTE IMMEDIATE 'DROP SEQUENCE ' || seqName;
          |  END LOOP;
          | END;""".stripMargin
    }

  override def autoInc(ci: ColumnInfo) = ""

  override def createForeignKey(sourceTable: TableInfo, name: String, sourceColumns: Seq[FieldSymbol], targetTable: TableInfo, targetColumns: Seq[FieldSymbol], onUpdate: ForeignKeyAction, onDelete: ForeignKeyAction): String = {
    val constraint = new StringBuilder(
      s"""alter table ${quoteTableName(sourceTable)}
      | add constraint ${quoteIdentifier(name)}
      | foreign key ${columnList(sourceColumns)}
      | references ${quoteTableName(targetTable)}
      | (${quotedColumnNames(targetColumns) mkString ", "})""".stripMargin
    )

    if (onDelete == ForeignKeyAction.Cascade)
      constraint append " on delete cascade "
    if (onDelete == ForeignKeyAction.SetNull)
      constraint append " on delete set null "
    if (onUpdate == ForeignKeyAction.Cascade)
      constraint append " initially deferred "

    constraint.toString
  }

  override def createIndex(index: IndexInfo) = {
    if (index.unique)
      s"""alter table ${quoteTableName(tableInfo(index.table))}
      | add constraint ${quoteIdentifier(index.name)} unique
      | ${columnList(index.columns)}""".stripMargin
    else super.createIndex(index)
  }

  override def dropPrimaryKey(table: TableInfo, name: String): String = {
    val searchPk = s"""SELECT UC.CONSTRAINT_NAME, UC.TABLE_NAME
                       | FROM USER_CONSTRAINTS UC
                       | WHERE CONSTRAINT_TYPE = 'P' and UC.CONSTRAINT_NAME = '${name.toUpperCase}'""".stripMargin

    s"""DECLARE
        |  constName varchar2(30);
        | BEGIN
        |  FOR results IN ($searchPk) LOOP
        |    constName := results.CONSTRAINT_NAME;
        |    EXECUTE IMMEDIATE 'ALTER TABLE  ${quoteTableName(table)} ' ||
        |    ' DROP CONSTRAINT ' || constName;
        |  END LOOP;
        | END;""".stripMargin
  }

  override def dropForeignKey(sourceTable: TableInfo, name: String): String = {
    val searchFk = s"""SELECT DISTINCT UC.CONSTRAINT_NAME
                       | FROM USER_CONSTRAINTS  UC
                       | INNER JOIN USER_CONS_COLUMNS UCC ON uc.R_CONSTRAINT_NAME = UCC.CONSTRAINT_NAME
                       | INNER JOIN USER_CONS_COLUMNS UCC2 ON UC.CONSTRAINT_NAME = UCC2.CONSTRAINT_NAME
                       | WHERE UC.CONSTRAINT_TYPE = 'R'
                       | AND UC.CONSTRAINT_NAME = '${name.toUpperCase}'""".stripMargin

    s"""DECLARE
        |  constName varchar2(30);
        | BEGIN
        |  FOR results IN ($searchFk) LOOP
        |    constName := results.CONSTRAINT_NAME;
        |    EXECUTE IMMEDIATE 'ALTER TABLE  ${quoteTableName(sourceTable)} ' ||
        |    ' DROP CONSTRAINT ' || constName;
        |  END LOOP;
        | END;""".stripMargin
  }

  override def dropIndex(index: IndexInfo) = {
    val searchUniqueIndex = s"""SELECT UC.CONSTRAINT_NAME, UC.TABLE_NAME
                                | FROM USER_CONSTRAINTS UC
                                | WHERE CONSTRAINT_TYPE = 'U' and UC.CONSTRAINT_NAME = '${index.name.toUpperCase}'""".stripMargin

    val searchIndex = s"""SELECT INDEX_NAME
                          | FROM USER_INDEXES
                          | WHERE INDEX_NAME = '${index.name.toUpperCase}'""".stripMargin

    if (index.unique) {
      s"""DECLARE
            |  constName varchar2(30);
            | BEGIN
            |  FOR results IN ($searchUniqueIndex) LOOP
            |    constName := results.CONSTRAINT_NAME;
            |    EXECUTE IMMEDIATE 'ALTER TABLE  ${quoteTableName(tableInfo(index.table))} ' ||
            |    ' DROP CONSTRAINT ' || constName;
            |  END LOOP;
            | END;""".stripMargin
    } else {
      s"""DECLARE
            |  idxName varchar2(30);
            | BEGIN
            |  FOR results IN ($searchIndex) LOOP
            |    idxName := results.INDEX_NAME;
            |    EXECUTE IMMEDIATE 'DROP INDEX ' || idxName;
            |  END LOOP;
            | END;""".stripMargin
    }
  }

  override def renameIndex(old: IndexInfo, newName: String): List[String] = {
    if (old.unique) super.renameIndex(old, newName) ++ List(
      s"""alter table ${quoteTableName(tableInfo(old.table))}
        | rename constraint ${quoteIdentifier(old.name)} to ${quoteIdentifier(newName)}""".stripMargin)
    else super.renameIndex(old, newName)
  }

  override def addColumn(table: TableInfo, column: ColumnInfo): String = {
    s"""alter table ${quoteTableName(table)}
    | add ${columnSql(column, newTable = false)}""".stripMargin
  }

  override def dropColumn(table: TableInfo, column: String): List[String] =
    super.dropColumn(table, column) :::
      dropTriggerAndSequence(table, column) :: Nil

  override def renameColumn(table: TableInfo, from: String, to: String): String = {
    s"""alter table ${quoteTableName(table)}
    | rename column ${quoteIdentifier(from)} to ${quoteIdentifier(to)}""".stripMargin
  }

  override def alterColumnType(table: TableInfo, column: ColumnInfo): List[String] = {
    val tmpColumnName = "temp_column"+(math.random*1000000).toInt
    val tmpColumn = column.copy(name = tmpColumnName)

    addColumn(table, tmpColumn) ::
    s"update ${quoteTableName(table)} set ${quoteIdentifier(tmpColumnName)} = ${quoteIdentifier(column.name)}" ::
    dropColumn(table, column.name) :::
    renameColumn(table, tmpColumn.name, column.name) :: Nil
  }

  override def alterColumnDefault(table: TableInfo, column: ColumnInfo): String =
    s"""alter table ${quoteTableName(table)}
    | modify (${quoteIdentifier(column.name)} default ${column.default getOrElse "null"})""".stripMargin

  override def alterColumnNullability(table: TableInfo, column: ColumnInfo): String =
    s"""alter table ${quoteTableName(table)}
    | modify (${quoteIdentifier(column.name)} ${if (column.notNull) "not null" else "null"})""".stripMargin
}

object GenericDialect {

  def apply(driver: JdbcProfile): Dialect[_ <: JdbcProfile] = driver match {
    case _: DerbyProfile    => new DerbyDialect
    case _: H2Profile       => new H2Dialect
    case _: SQLiteProfile   => new SQLiteDialect
    case _: HsqldbProfile   => new HsqldbDialect
    case _: MySQLProfile    => new MySQLDialect
    case _: PostgresProfile => new PostgresDialect
    case _: OracleProfile   => new OracleDialect
    case _ =>
      throw new IllegalArgumentException("Slick error : Unknown or unsupported jdbc driver found.")
  }
}
