package scala.slick
package migration.api

import driver.JdbcDriver
import lifted._
import ast._

case class TableInfo(schemaName: Option[String], tableName: String)

case class ColumnInfo(name: String, sqlType: String, notNull: Boolean, autoInc: Boolean, isPk: Boolean, default: Option[String])

case class IndexInfo(table: TableNode, name: String, unique: Boolean, columns: Seq[FieldSymbol])

trait TableMigrationBase {
  def table: TableInfo

  // not reversible:
  def tableDrop: Boolean = false
  // reverse: create=false, drop=true
  def tableCreate: Boolean = false
  // reverse: reverse rename
  def tableRename: Option[String] = None
  // reverse: drop instead of create
  def columnsCreate: Seq[ColumnInfo] = Nil
  // reverse: create instead of drop
  def columnsDrop: Seq[ColumnInfo] = Nil
  // reverse: reverse rename
  def columnsRename: Map[ColumnInfo, String] = Map.empty
  // not reversible: insufficient info: don't have old data
  def columnsAlterType: Seq[ColumnInfo] = Nil
  // not reversible: insufficient info: don't have old data
  def columnsAlterDefault: Seq[ColumnInfo] = Nil
  // not reversible: insufficient info: don't have old data
  def columnsAlterNullability: Seq[ColumnInfo] = Nil
  // reverse: drop instead of create
  def foreignKeysCreate: Seq[ForeignKey[_ <: TableNode, _]] = Nil
  // not reversible: insufficient info
  def foreignKeysDrop: Seq[ForeignKey[_ <: TableNode, _]] = Nil
  //TODO is multiple pks possible?
  // reverse: drop instead of create
  def primaryKeysCreate: Seq[(String, Seq[FieldSymbol])] = Nil
  // reverse: create instead of drop
  def primaryKeysDrop: Seq[(String, Seq[FieldSymbol])] = Nil
  // reverse: drop instead of create
  def indexesCreate: Seq[IndexInfo] = Nil
  // reverse: create instead of drop
  def indexesDrop: Seq[IndexInfo] = Nil
  // reverse: reverse rename
  def indexesRename: Map[IndexInfo, String] = Map.empty
}

trait AstHelpers {
  protected def tableInfo(table: TableNode): TableInfo = TableInfo(table.schemaName, table.tableName)

  protected def fieldSym(node: Node): Option[FieldSymbol] = node match {
    case Select(_, f: FieldSymbol) => Some(f)
    case _                         => None
  }

  protected def fieldSym(column: Column[_]): FieldSymbol =
    fieldSym(Node(column)) getOrElse sys.error("Invalid column: " + column)

  protected def indexInfo(index: Index) = IndexInfo(index.table, index.name, index.unique, index.on flatMap (fieldSym(_)))
}

class Migrations[D <: JdbcDriver](val driver: D)(implicit dialect: Dialect[D]) extends AstHelpers {
  private def columnInfo(column: FieldSymbol): ColumnInfo = {
    val ti       = driver.typeInfoFor(column.tpe)
    val initial  = ColumnInfo(column.name, ti.sqlTypeName, !ti.nullable, false, false, None)
    column.options.foldLeft(initial) {
      case (ci, ColumnOption.DBType(s))   => ci.copy(sqlType = s)
      case (ci, ColumnOption.NotNull)     => ci.copy(notNull = true)
      case (ci, ColumnOption.Nullable)    => ci.copy(notNull = false)
      case (ci, ColumnOption.AutoInc)     => ci.copy(autoInc = true)
      case (ci, ColumnOption.PrimaryKey)  => ci.copy(isPk = true)
      case (ci, ColumnOption.Default(v))  => ci.copy(default = Some(ti.valueToSQLLiteral(v)))
      case (ci, _)                        => ci
    }
  }
  private def columnInfo(column: Column[_]): ColumnInfo =
    fieldSym(Node(column)) match {
      case Some(c) => columnInfo(c)
      case None    => sys.error("Invalid column: " + column)
    }

  trait Migration {
    def apply()(implicit session: driver.simple.Session): Unit
  }

  object Migration {
    implicit class MigrationConcat[M <: Migration](m: M) {
      def &[N <: Migration, O](n: N)(implicit ccm: CanConcatMigrations[M, N, O]): O = ccm.f(m, n)
    }
  }

  trait ReversibleMigration extends Migration {
    def reverse: Migration
  }

  class CanConcatMigrations[-A, -B, +C](val f: (A, B) => C)
  class CanConcatMigrationsLow {
    implicit def default[A <: Migration, B <: Migration]: CanConcatMigrations[A, B, MigrationSeq] =
      new CanConcatMigrations({
        case (MigrationSeq(as @ _*), b) => MigrationSeq(as :+ b: _*)
        case (a, b)                     => MigrationSeq(a, b)
      })
  }
  object CanConcatMigrations extends CanConcatMigrationsLow {
    implicit def reversible[A <: ReversibleMigration, B <: ReversibleMigration]: CanConcatMigrations[A, B, ReversibleMigrationSeq] = new CanConcatMigrations({
      case (rms: ReversibleMigrationSeq, b) => new ReversibleMigrationSeq(rms.migrations :+ b: _*)
      case (a, b)                           => new ReversibleMigrationSeq(a, b)
    })
  }

  case class MigrationSeq(migrations: Migration*) extends Migration {
    final def apply()(implicit session: driver.simple.Session) = migrations foreach (_())
  }

  class ReversibleMigrationSeq(override val migrations: ReversibleMigration*) extends MigrationSeq(migrations: _*) with ReversibleMigration {
    def reverse = MigrationSeq(migrations.reverse.map(_.reverse): _*)
  }

  trait SqlMigration extends Migration {
    def sql: Seq[String]
    def apply()(implicit session: driver.simple.Session) = {
      val sq = sql
      session.withTransaction {
        session.withStatement() { st =>
          for(s <- sq)
            try st execute s
            catch {
              case e: java.sql.SQLException =>
                throw MigrationException(s"Could not execute sql: '$s'", e)
            }
        }
      }
    }
  }

  //TODO mechanism other than exceptions?
  case class MigrationException(message: String, cause: Throwable) extends RuntimeException(message, cause)

  object SqlMigration {
    def apply(sql: String) = {
      def sql0 = sql
      new SqlMigration {
        def sql = Seq(sql0)
      }
    }
  }

  object TableMigration {
    def apply[T <: TableNode](tableNode: T) = new TableMigration(tableNode)
  }
  class TableMigration[T <: TableNode](tableNode: T)
    extends TableMigrationBase
    with SqlMigration { outer =>

    def sql = dialect.migrateTable(this)

    def table = TableInfo(tableNode.schemaName, tableNode.tableName)

    protected def copy(
      tblDrop: Boolean = tableDrop,
      tblCreate: Boolean = tableCreate,
      tblRename: Option[String] = tableRename,
      colCreate: Seq[ColumnInfo] = columnsCreate,
      colDrop: Seq[ColumnInfo] = columnsDrop,
      colRename: Map[ColumnInfo, String] = columnsRename,
      colAlterType: Seq[ColumnInfo] = columnsAlterType,
      colAlterDefault: Seq[ColumnInfo] = columnsAlterDefault,
      colAlterNullability: Seq[ColumnInfo] = columnsAlterNullability,
      frgnKeysCreate: Seq[ForeignKey[_ <: TableNode, _]] = foreignKeysCreate,
      frgnKeysDrop: Seq[ForeignKey[_ <: TableNode, _]] = foreignKeysDrop,
      prmryKeysCreate: Seq[(String, Seq[FieldSymbol])] = primaryKeysCreate,
      prmryKeysDrop: Seq[(String, Seq[FieldSymbol])] = primaryKeysDrop,
      idxCreate: Seq[IndexInfo] = indexesCreate,
      idxDrop: Seq[IndexInfo] = indexesDrop,
      idxRename: Map[IndexInfo, String] = indexesRename
    ): TableMigration[T] = new TableMigration[T](tableNode) {
      override def tableDrop: Boolean = tblDrop
      override def tableCreate: Boolean = tblCreate
      override def tableRename: Option[String] = tblRename
      override def columnsCreate: Seq[ColumnInfo] = colCreate
      override def columnsDrop: Seq[ColumnInfo] = colDrop
      override def columnsRename: Map[ColumnInfo, String] = colRename
      override def columnsAlterType: Seq[ColumnInfo] = colAlterType
      override def columnsAlterDefault: Seq[ColumnInfo] = colAlterDefault
      override def columnsAlterNullability: Seq[ColumnInfo] = colAlterNullability
      override def foreignKeysCreate: Seq[ForeignKey[_ <: TableNode, _]] = frgnKeysCreate
      override def foreignKeysDrop: Seq[ForeignKey[_ <: TableNode, _]] = frgnKeysDrop
      override def primaryKeysCreate: Seq[(String, Seq[FieldSymbol])] = prmryKeysCreate
      override def primaryKeysDrop: Seq[(String, Seq[FieldSymbol])] = prmryKeysDrop
      override def indexesCreate: Seq[IndexInfo] = idxCreate
      override def indexesDrop: Seq[IndexInfo] = idxDrop
      override def indexesRename: Map[IndexInfo, String] = idxRename
    }

    private def colInfo(f: T => Column[_]) = columnInfo(f(tableNode))

    def create = copy(
      tblCreate = true
    )

    def drop = copy(
      tblCreate = false,
      tblDrop = true
    )

    def rename(to: String) = copy(
      tblRename = Some(to)
    )

    def addColumns(cols: (T => Column[_])*) = copy(
      colCreate = columnsCreate ++
        cols.map(colInfo)
    )

    def dropColumns(cols: (T => Column[_])*) = copy(
      colDrop = columnsDrop ++
        cols.map(colInfo)
    )

    def renameColumn(col: T => Column[_], to: String) = copy(
      colRename = columnsRename +
        (colInfo(col) -> to)
    )

    def alterColumnTypes(cols: (T => Column[_])*) = copy(
      colAlterType = columnsAlterType ++
        cols.map(colInfo)
    )

    def alterColumnDefaults(cols: (T => Column[_])*) = copy(
      colAlterDefault = columnsAlterDefault ++
        cols.map(colInfo)
    )

    def alterColumnNulls(cols: (T => Column[_])*) = copy(
      colAlterNullability = columnsAlterNullability ++
        cols.map(colInfo)
    )

    def addPrimaryKeys(pks: (T => PrimaryKey)*) = copy(
      prmryKeysCreate = primaryKeysCreate ++
        pks.map { f =>
          val key = f(tableNode)
          (key.name, key.columns flatMap (fieldSym(_)))
        }
    )

    def dropPrimaryKeys(pks: (T => PrimaryKey)*) = copy(
      prmryKeysDrop = primaryKeysDrop ++
        pks.map { f =>
          val key = f(tableNode)
          (key.name, key.columns flatMap (fieldSym(_)))
        }
    )

    def addForeignKeys(fkqs: (T => ForeignKeyQuery[_ <: TableNode, _])*) = copy(
      frgnKeysCreate = foreignKeysCreate ++
        fkqs.flatMap { f =>
          val fkq = f(tableNode)
          fkq.fks: Seq[ForeignKey[_ <: TableNode, _]]
        }
    )

    def dropForeignKeys(fkqs: (T => ForeignKeyQuery[_ <: TableNode, _])*) = copy(
      frgnKeysDrop = foreignKeysDrop ++
        fkqs.flatMap { f =>
          val fkq = f(tableNode)
          fkq.fks: Seq[ForeignKey[_ <: TableNode, _]]
        }
      )

    def addIndexes(indexes: (T => Index)*) = copy(
      idxCreate = indexesCreate ++
        indexes.map { f =>
          val i = f(tableNode)
          indexInfo(i)
        }
    )

    def dropIndexes(indexes: (T => Index)*) = copy(
      idxDrop = indexesDrop ++
        indexes.map { f =>
          val i = f(tableNode)
          indexInfo(i)
        }
    )

    def renameIndex(index: (T => Index), to: String) = copy(
      idxRename = indexesRename +
        (indexInfo(index(tableNode)) -> to)
    )
  }
}
