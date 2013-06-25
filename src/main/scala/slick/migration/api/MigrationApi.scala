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
      case (ci, ColumnOption.Default(v)) => ci.copy(default = Some(ti/*.asInstanceOf[TypeMapperDelegate[Any]]*/.valueToSQLLiteral(v)))
      case (ci, _)                       => ci
    }
  }

  protected def fieldSym(node: Node): Option[FieldSymbol] = node match {
    case Select(_, f: FieldSymbol) => Some(f)
    case _                         => None
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
    //TODO withPrimaryKeys
    //TODO withIndexes
  }

  case class CreateTable[T <: TableNode](table: T)(columns: (T => Column[_])*) extends SqlMigration with CreateTableBase[T] {
    protected val fss = columns flatMap (f => fieldSym(Node(f(table))))

    def sql = dialect.createTable(table, fss map columnInfo)

    def reverse = DropTable(table)
  }

  case class DropTable(table: TableNode) extends SqlMigration {
    def sql = dialect.dropTable(table)
  }

  object CreateForeignKey {
    def apply(fkq: ForeignKeyQuery[_ <: TableNode, _]): ReversibleMigrationSeq =
     new ReversibleMigrationSeq(fkq.fks.map(new CreateForeignKey(_)): _*)
  }
  case class CreateForeignKey(fk: ForeignKey[_ <: TableNode, _]) extends SqlMigration with ReversibleMigration {
    def sql = fk.sourceTable match {
      case sourceTable: TableNode =>
        dialect.createForeignKey(sourceTable, fk.name, fk.linearizedSourceColumns.flatMap(fieldSym), fk.targetTable, fk.linearizedTargetColumnsForOriginalTargetTable.flatMap(fieldSym), fk.onUpdate, fk.onDelete)
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
}
