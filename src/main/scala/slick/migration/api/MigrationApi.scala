package scala.slick
package migration.api

import driver.JdbcDriver
import lifted._
import ast._

case class ColumnInfo(name: String, sqlType: String, notNull: Boolean, autoInc: Boolean, isPk: Boolean, default: Option[String])

class Migrations[D <: JdbcDriver](val driver: D)(implicit hasDialect: HasDialect[D]) {

  val dialect = hasDialect.f(driver)

  protected def columnInfo(column: FieldSymbol): ColumnInfo = {
    val ti = driver.typeInfoFor(column.tpe)
    val initial = ColumnInfo(column.name, ti.sqlTypeName, !ti.nullable, false, false, None)
    column.options.foldLeft(initial){
      case (ci, ColumnOption.DBType(s))  => ci.copy(sqlType = s)
      case (ci, ColumnOption.NotNull)    => ci.copy(notNull = true)
      case (ci, ColumnOption.Nullable)   => ci.copy(notNull = false)
      case (ci, ColumnOption.AutoInc)    => ci.copy(autoInc = true)
      case (ci, ColumnOption.PrimaryKey) => ci.copy(isPk = true)
      case (ci, ColumnOption.Default(v)) => ci.copy(default = Some(ti.valueToSQLLiteral(v)))
      case (ci, _)                       => ci
    }
  }
  protected def columnInfo(column: Column[_]): ColumnInfo =
    fieldSym(Node(column)) match {
      case Some(c) => columnInfo(c)
      case None    => sys.error("Invalid column: " + column)
    }

  protected def fieldSym(node: Node): Option[FieldSymbol] = node match {
    case Select(_, f: FieldSymbol) => Some(f)
    case _                         => None
  }

  protected def fieldSym(column: Column[_]): FieldSymbol =
    fieldSym(Node(column)) getOrElse sys.error("Invalid column: " + column)

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
    def sql: String
    def apply()(implicit session: driver.simple.Session) = session.withStatement()(_ execute sql)
  }

  object SqlMigration {
    def apply(sql: String) = {
      def sql0 = sql
      new SqlMigration {
        def sql = sql0
      }
    }
  }
  trait CreateTableBase[T <: TableNode] extends ReversibleMigration { outer =>
    class Wrapper(additional: ReversibleMigrationSeq) extends CreateTableBase[T] {
      def table = outer.table
      def all = outer & additional
      def apply()(implicit session: driver.simple.Session) = all()
      def reverse = all.reverse
    }
    def table: T

    def withForeignKeys(fks: (T => ForeignKeyQuery[_ <: TableNode, _])*) = new Wrapper(
      new ReversibleMigrationSeq(fks.map(f => CreateForeignKey(f(table))): _*)
    )

    def withPrimaryKeys(pks: (T => PrimaryKey)*) = new Wrapper(
      new ReversibleMigrationSeq(pks.map(f => CreatePrimaryKey(table)(f)): _*)
    )

    def withIndexes(idxs: (T => Index)*) = new Wrapper(
      new ReversibleMigrationSeq(idxs.map(f => CreateIndex(f(table))): _*)
    )
  }

  case class CreateTable[T <: TableNode](table: T)(columns: (T => Column[_])*) extends SqlMigration with CreateTableBase[T] {
    protected val fss = columns flatMap (f => fieldSym(Node(f(table))))

    def sql = dialect.createTable(table, fss map columnInfo)

    def reverse = DropTable(table)
  }

  case class DropTable(table: TableNode) extends SqlMigration {
    def sql = dialect.dropTable(table)
  }

  case class RenameTable(table: TableNode, to: String) extends SqlMigration {
    def sql = dialect.renameTable(table, to)
  }

  object CreateForeignKey {
    def apply(fkq: ForeignKeyQuery[_ <: TableNode, _]): ReversibleMigrationSeq =
      new ReversibleMigrationSeq(fkq.fks.map(new CreateForeignKey(_)): _*)
  }
  case class CreateForeignKey(fk: ForeignKey[_ <: TableNode, _]) extends SqlMigration with ReversibleMigration {
    def sql = fk.sourceTable match {
      case sourceTable: TableNode =>
        dialect.createForeignKey(sourceTable, fk.name, fk.linearizedSourceColumns.flatMap(fieldSym(_).toSeq), fk.targetTable, fk.linearizedTargetColumnsForOriginalTargetTable.flatMap(fieldSym(_).toSeq), fk.onUpdate, fk.onDelete)
    }

    def reverse = DropForeignKey(fk)
  }

  object DropForeignKey {
    def apply(fkq: ForeignKeyQuery[_ <: TableNode, _]): ReversibleMigrationSeq =
      new ReversibleMigrationSeq(fkq.fks.map(new DropForeignKey(_)): _*)
  }
  case class DropForeignKey(fk: ForeignKey[_ <: TableNode, _]) extends SqlMigration with ReversibleMigration {
    def sql = fk.sourceTable match {
      case sourceTable: TableNode =>
        dialect.dropForeignKey(sourceTable, fk.name)
    }
    def reverse = CreateForeignKey(fk)
  }

  case class CreatePrimaryKey[T <: TableNode](table: T)(key: T => PrimaryKey) extends SqlMigration with ReversibleMigration {
    def sql = {
      val pk = key(table)
      dialect.createPrimaryKey(table, pk.name, pk.columns flatMap (fieldSym(_)))
    }
    def reverse = DropPrimaryKey(table)(key)
  }

  case class DropPrimaryKey[T <: TableNode](table: T)(key: T => PrimaryKey) extends SqlMigration with ReversibleMigration {
    def sql = dialect.dropPrimaryKey(table, key(table).name)
    def reverse = CreatePrimaryKey(table)(key)
  }

  case class CreateIndex(index: Index) extends SqlMigration with ReversibleMigration {
    def sql = dialect.createIndex(index.table, index.name, index.unique, index.on flatMap (fieldSym(_)))
    def reverse = DropIndex(index)
  }

  case class DropIndex(index: Index) extends SqlMigration with ReversibleMigration {
    def sql = dialect.dropIndex(index.name)
    def reverse = CreateIndex(index)
  }

  case class RenameIndex(index: Index, to: String) extends SqlMigration {
    def sql = dialect.renameIndex(index.name, to)
  }

  case class AddColumn[T <: TableNode](table: T)(column: T => Column[_]) extends SqlMigration with ReversibleMigration {
    def sql = dialect.addColumn(table, columnInfo(column(table)))
    def reverse = DropColumn(table)(column)
  }
  case class DropColumn[T <: TableNode](table: T)(column: T => Column[_]) extends SqlMigration with ReversibleMigration {
    def sql = dialect.dropColumn(table, fieldSym(column(table)))
    def reverse = AddColumn(table)(column)
  }

  case class RenameColumn[T <: TableNode](table: T)(oldColumn: T => Column[_], newColumn: T => Column[_]) extends SqlMigration {
    def sql = dialect.renameColumn(table, fieldSym(oldColumn(table)), fieldSym(newColumn(table)))
  }

  case class AlterColumnType[T <: TableNode](table: T)(column: T => Column[_]) extends SqlMigration {
    def sql = dialect.alterColumnType(table, columnInfo(column(table)))
  }
  case class AlterColumnDefault[T <: TableNode](table: T)(column: T => Column[_]) extends SqlMigration {
    def sql = dialect.alterColumnDefault(table, columnInfo(column(table)))
  }
  case class AlterColumnNullability[T <: TableNode](table: T)(column: T => Column[_]) extends SqlMigration {
    def sql = dialect.alterColumnNullability(table, columnInfo(column(table)))
  }
}
