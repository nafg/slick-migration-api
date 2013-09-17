package scala.slick
package migration.api

import scala.slick.driver.JdbcDriver
import scala.slick.ast.{ FieldSymbol, Node, TableNode }
import scala.slick.lifted.{ Column, ForeignKey, ForeignKeyQuery, Index, PrimaryKey }

case class TableMigrationData(
  // not reversible: insufficient info: don't know entire old table
  tableDrop: Boolean = false,
  // reverse: create=false, drop=true
  tableCreate: Boolean = false,
  // reverse: reverse rename
  tableRename: Option[String] = None,
  // reverse: drop instead of create
  columnsCreate: Seq[ColumnInfo] = Nil,
  // reverse: create instead of drop
  columnsDrop: Seq[ColumnInfo] = Nil,
  // reverse: reverse rename
  columnsRename: Map[ColumnInfo, String] = Map.empty,
  // not reversible: insufficient info: don't have old data
  columnsAlterType: Seq[ColumnInfo] = Nil,
  // not reversible: insufficient info: don't have old data
  columnsAlterDefault: Seq[ColumnInfo] = Nil,
  // not reversible: insufficient info: don't have old data
  columnsAlterNullability: Seq[ColumnInfo] = Nil,
  // reverse: drop instead of create
  foreignKeysCreate: Seq[ForeignKey[_ <: TableNode, _]] = Nil,
  // reverse: create instead of drop
  foreignKeysDrop: Seq[ForeignKey[_ <: TableNode, _]] = Nil,
  // reverse: drop instead of create
  primaryKeysCreate: Seq[(String, Seq[FieldSymbol])] = Nil,
  // reverse: create instead of drop
  primaryKeysDrop: Seq[(String, Seq[FieldSymbol])] = Nil,
  // reverse: drop instead of create
  indexesCreate: Seq[IndexInfo] = Nil,
  // reverse: create instead of drop
  indexesDrop: Seq[IndexInfo] = Nil,
  // reverse: reverse rename
  indexesRename: Map[IndexInfo, String] = Map.empty
)

class TableMigrations[D <: JdbcDriver](driver: D)(implicit dialect: Dialect[D]) {

  object TableMigration {
    def apply[T <: TableNode](tableNode: T) = new ReversibleTableMigration(tableNode, TableMigrationData())
  }
  sealed abstract class TableMigration[T <: TableNode](tableNode: T)
    extends SqlMigration with AstHelpers with Equals { outer =>
    type Self <: TableMigration[T]

    def sql = dialect.migrateTable(table, data)

    def table = TableInfo(tableNode.schemaName, tableNode.tableName)

    protected[api] def data: TableMigrationData

    protected def withData(data: TableMigrationData): Self

    private def colInfo(f: T => Column[_]): ColumnInfo = {
      val col = f(tableNode)
      fieldSym(Node(col)) match {
        case Some(c) => columnInfo(driver, c)
        case None    => sys.error("Invalid column: " + col)
      }
    }

    def create = withData(data.copy(
      tableCreate = true
    ))

    def drop = new IrreversibleTableMigration(
      tableNode,
      table,
      data.copy(
        tableCreate = false,
        tableDrop = true
      )
    )

    def rename(to: String) = withData(data.copy(
      tableRename = Some(to)
    ))

    def addColumns(cols: (T => Column[_])*) = withData(data.copy(
      columnsCreate = data.columnsCreate ++
        cols.map(colInfo)
    ))

    def dropColumns(cols: (T => Column[_])*) = withData(data.copy(
      columnsDrop = data.columnsDrop ++
        cols.map(colInfo)
    ))

    def renameColumn(col: T => Column[_], to: String) = withData(data.copy(
      columnsRename = data.columnsRename +
        (colInfo(col) -> to)
    ))

    def alterColumnTypes(cols: (T => Column[_])*) = new IrreversibleTableMigration(
      tableNode,
      table,
      data.copy(
        columnsAlterType = data.columnsAlterType ++
          cols.map(colInfo)
      )
    )

    def alterColumnDefaults(cols: (T => Column[_])*) = new IrreversibleTableMigration(
      tableNode,
      table,
      data.copy(
        columnsAlterDefault = data.columnsAlterDefault ++
          cols.map(colInfo)
      )
    )

    def alterColumnNulls(cols: (T => Column[_])*) = new IrreversibleTableMigration(
      tableNode,
      table,
      data.copy(
        columnsAlterNullability = data.columnsAlterNullability ++
          cols.map(colInfo)
      )
    )

    def addPrimaryKeys(pks: (T => PrimaryKey)*) = withData(data.copy(
      primaryKeysCreate = data.primaryKeysCreate ++
        pks.map { f =>
          val key = f(tableNode)
          (key.name, key.columns flatMap (fieldSym(_)))
        }
    ))

    def dropPrimaryKeys(pks: (T => PrimaryKey)*) = withData(data.copy(
      primaryKeysDrop = data.primaryKeysDrop ++
        pks.map { f =>
          val key = f(tableNode)
          (key.name, key.columns flatMap (fieldSym(_)))
        }
    ))

    def addForeignKeys(fkqs: (T => ForeignKeyQuery[_ <: TableNode, _])*) = withData(data.copy(
      foreignKeysCreate = data.foreignKeysCreate ++
        fkqs.flatMap { f =>
          val fkq = f(tableNode)
          fkq.fks: Seq[ForeignKey[_ <: TableNode, _]]
        }
    ))

    def dropForeignKeys(fkqs: (T => ForeignKeyQuery[_ <: TableNode, _])*) = withData(data.copy(
      foreignKeysDrop = data.foreignKeysDrop ++
        fkqs.flatMap { f =>
          val fkq = f(tableNode)
          fkq.fks: Seq[ForeignKey[_ <: TableNode, _]]
        }
    ))

    def addIndexes(indexes: (T => Index)*) = withData(data.copy(
      indexesCreate = data.indexesCreate ++
        indexes.map { f =>
          val i = f(tableNode)
          indexInfo(i)
        }
    ))

    def dropIndexes(indexes: (T => Index)*) = withData(data.copy(
      indexesDrop = data.indexesDrop ++
        indexes.map { f =>
          val i = f(tableNode)
          indexInfo(i)
        }
    ))

    def renameIndex(index: (T => Index), to: String) = withData(data.copy(
      indexesRename = data.indexesRename +
        (indexInfo(index(tableNode)) -> to)
    ))

    def canEqual(that: Any) = that.isInstanceOf[TableMigration[_]]

    override def equals(a: Any) = a match {
      case that: TableMigration[_] if that canEqual this =>
        (that.table, that.data) == (this.table, this.data)
      case _ => false
    }
  }

  final class IrreversibleTableMigration[T <: TableNode] private[TableMigrations](tableNode: T, override val table: TableInfo, protected[api] val data: TableMigrationData) extends TableMigration[T](tableNode) {
    type Self = IrreversibleTableMigration[T]
    protected def withData(d: TableMigrationData) = new IrreversibleTableMigration(tableNode, table, d)
  }

  final class ReversibleTableMigration[T <: TableNode] private[TableMigrations](tableNode: T, protected[api] val data: TableMigrationData) extends TableMigration[T](tableNode) with ReversibleMigration { outer =>

    require(data.tableDrop == false)
    require(data.columnsAlterType.isEmpty)
    require(data.columnsAlterDefault.isEmpty)
    require(data.columnsAlterNullability.isEmpty)

    type Self = ReversibleTableMigration[T]
    protected def withData(d: TableMigrationData) = new ReversibleTableMigration(tableNode, d)

    def reverse = {
      new IrreversibleTableMigration(
        tableNode,
        outer.data.tableRename match {
          case Some(name) => outer.table.copy(tableName = name)
          case None       => outer.table
        },
        data.copy(
          tableDrop               = data.tableCreate,
          tableCreate             = false,
          tableRename             = data.tableRename.map(_ => table.tableName),
          columnsCreate           = data.columnsDrop.reverse,
          columnsDrop             = data.columnsCreate.reverse,
          columnsRename           = data.columnsRename.map {
            case (ci, s) => (ci.copy(name = s), ci.name)
          },
          columnsAlterType        = Nil,
          columnsAlterDefault     = Nil,
          columnsAlterNullability = Nil,
          foreignKeysCreate       = data.foreignKeysDrop.reverse,
          foreignKeysDrop         = data.foreignKeysCreate.reverse,
          primaryKeysCreate       = data.primaryKeysDrop.reverse,
          primaryKeysDrop         = data.primaryKeysCreate.reverse,
          indexesCreate           = data.indexesDrop.reverse,
          indexesDrop             = data.indexesCreate.reverse,
          indexesRename           = data.indexesRename.map {
            case (ii, s) => (ii.copy(name = s), ii.name)
          }
        )
      )
    }
  }
}
