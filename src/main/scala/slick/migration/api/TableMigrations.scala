package scala.slick
package migration.api

import scala.slick.driver.JdbcDriver
import scala.slick.ast.{ FieldSymbol, Node, TableNode }
import scala.slick.lifted.{ Column, ForeignKey, ForeignKeyQuery, Index, PrimaryKey }

class TableMigrations[D <: JdbcDriver](driver: D)(implicit dialect: Dialect[D]) {
  object TableMigration {
    def apply[T <: TableNode](tableNode: T) = new TableMigration(tableNode)
  }
  class TableMigration[T <: TableNode](tableNode: T)
    extends SqlMigration with AstHelpers { outer =>
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

    private def colInfo(f: T => Column[_]): ColumnInfo = {
      val col = f(tableNode)
      fieldSym(Node(col)) match {
        case Some(c) => columnInfo(driver, c)
        case None    => sys.error("Invalid column: " + col)
      }
    }

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
