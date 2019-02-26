package slick.migration.api

import slick.jdbc.{JdbcProfile, JdbcType}
import slick.lifted._
import slick.migration.api.AstHelpers.{ColumnInfo, IndexInfo, PrimaryKeyInfo, TableInfo}


object TableMigration {
  sealed abstract class Action(val sort: Int)

  object Action {
    sealed abstract class Reversible(sort: Int) extends Action(sort)
    case object DropTable extends Action(0)
    case object CreateTable extends Reversible(1)
    case class RenameTableTo(to: String) extends Reversible(2)
    case class RenameTableFrom(from: String) extends Reversible(2)
    case class AddColumn(info: ColumnInfo) extends Reversible(3)
    case class AddColumnAndSetInitialValue(info: ColumnInfo, rawSqlExpr: String) extends Reversible(3)
    case class DropColumn(info: ColumnInfo) extends Reversible(4)
    case class DropColumnOfName(name: String) extends Action(4)
    case class RenameColumnTo(originalInfo: ColumnInfo, to: String) extends Reversible(5)
    case class RenameColumnFrom(currentInfo: ColumnInfo, from: String) extends Reversible(5)
    case class AlterColumnType(info: ColumnInfo) extends Action(6)
    case class AlterColumnDefault(info: ColumnInfo) extends Action(6)
    case class AlterColumnNullable(info: ColumnInfo) extends Action(6)
    case class DropPrimaryKey(info: PrimaryKeyInfo) extends Reversible(7)
    case class DropForeignKey(fk: ForeignKey) extends Reversible(7)
    case class DropIndex(info: IndexInfo) extends Reversible(7)
    case class AddPrimaryKey(info: PrimaryKeyInfo) extends Reversible(8)
    case class AddForeignKey(fk: ForeignKey) extends Reversible(8)
    case class CreateIndex(info: IndexInfo) extends Reversible(8)
    case class RenameIndexTo(originalInfo: IndexInfo, to: String) extends Reversible(9)
    case class RenameIndexFrom(currentInfo: IndexInfo, from: String) extends Reversible(9)
  }

  sealed trait WithActions[A <: Action] {
    def apply(actions: Seq[Action.Reversible]): List[A] => List[A]
    def apply(actions: Seq[Action], dummy: Null = null): List[A] => List[Action]
    def apply(action: Action.Reversible): List[A] => List[A]
    def apply(action: Action): List[A] => List[Action]
  }

  trait WithActionsLow {
    implicit val default: WithActions[Action] = new WithActions[Action] {
      override def apply(actions: Seq[Action.Reversible]) = actions ++: _
      override def apply(actions: Seq[Action], dummy: Null) = actions ++: _
      override def apply(action: Action.Reversible) = action :: _
      override def apply(action: Action) = action :: _
    }
  }

  object WithActions extends WithActionsLow {
    implicit val reversible: WithActions[Action.Reversible] = new WithActions[Action.Reversible] {
      override def apply(actions: Seq[Action.Reversible]) = actions ++: _
      override def apply(actions: Seq[Action], dummy: Null) = actions ++: _
      override def apply(action: Action.Reversible) = action :: _
      override def apply(action: Action) = action :: _
    }
  }
  implicit class TableMigrationReversible[T <: JdbcProfile#Table[_]](self: TableMigration[T, Action.Reversible])
                                                                    (implicit dialect: Dialect[_])
    extends ReversibleMigration {

    override def apply() = self.apply()

    override def reverse = {
      val tm0 = self.modActions(_ => List.empty[Action])
      self.actions.foldLeft(tm0) { (tm, action) =>
        action match {
          case Action.CreateTable                          => tm.modActions(Action.DropTable :: _)
          case Action.RenameTableTo(to)                    => tm.modActions(Action.RenameTableFrom(to) :: _)
          case Action.RenameTableFrom(from)                => tm.modActions(Action.RenameTableTo(from) :: _)
          case Action.AddColumn(info)                      => tm.modActions(Action.DropColumn(info) :: _)
          case Action.AddColumnAndSetInitialValue(info, _) => tm.modActions(Action.DropColumn(info) :: _)
          case Action.DropColumn(info)                     => tm.modActions(Action.AddColumn(info) :: _)
          case Action.RenameColumnTo(originalInfo, to)     => tm.modActions(Action.RenameColumnFrom(originalInfo, to) :: _)
          case Action.RenameColumnFrom(currentInfo, from)  => tm.modActions(Action.RenameColumnTo(currentInfo, from) :: _)
          case Action.DropPrimaryKey(info)                 => tm.modActions(Action.AddPrimaryKey(info) :: _)
          case Action.DropForeignKey(fk)                   => tm.modActions(Action.AddForeignKey(fk) :: _)
          case Action.DropIndex(info)                      => tm.modActions(Action.CreateIndex(info) :: _)
          case Action.AddPrimaryKey(info)                  => tm.modActions(Action.DropPrimaryKey(info) :: _)
          case Action.AddForeignKey(fk)                    => tm.modActions(Action.DropForeignKey(fk) :: _)
          case Action.CreateIndex(info)                    => tm.modActions(Action.DropIndex(info) :: _)
          case Action.RenameIndexTo(originalInfo, to)      => tm.modActions(Action.RenameIndexFrom(originalInfo, to) :: _)
          case Action.RenameIndexFrom(currentInfo, from)   => tm.modActions(Action.RenameIndexTo(currentInfo, from) :: _)

        }
      }
    }
  }

  def apply[T <: JdbcProfile#Table[_]](table: T)
                                      (implicit dialect: Dialect[_]): TableMigration[T, Action.Reversible] =
    new TableMigration(TableInfo(table.schemaName, table.tableName), Nil)(table)

  def apply[T <: JdbcProfile#Table[_]](tableQuery: TableQuery[T])
                                      (implicit dialect: Dialect[_]): TableMigration[T, Action.Reversible] =
    apply(tableQuery.baseTableRow)
}

import slick.migration.api.TableMigration.Action


case class TableMigration[T <: JdbcProfile#Table[_], A <: Action](tableInfo: TableInfo, actions: List[A])
                                                                 (val table: T)
                                                                 (implicit dialect: Dialect[_],
                                                                  withActions: TableMigration.WithActions[A])
  extends SqlMigration with AstHelpers {

  private def modActions[B <: Action : TableMigration.WithActions](f: List[A] => List[B]) =
    copy(actions = f(actions))(table)

  protected def columnInfo: (T => Rep[_]) => ColumnInfo = colInfo(table)

  def sql = dialect.migrateTable(tableInfo, actions)

  /**
   * Create the table.
   *
   * @group oper
   */
  def create =
    modActions(withActions(Action.CreateTable))

  /**
   * Drop the table.
   *
   * @group oper
   */
  def drop =
    modActions(withActions(Action.DropTable))

  /**
   * Rename the table
   *
   * @param to the new name for the table
   * @group oper
   */
  def rename(to: String) =
    modActions(withActions(Action.RenameTableTo(to)))

  def renameFrom(from: String) =
    modActions(withActions(Action.RenameTableFrom(from)))

  /**
   * Add columns to the table.
   * (If the table is being created, these may be incorporated into the `CREATE TABLE` statement.)
   *
   * @param cols zero or more column-returning functions, which are passed the table object.
   * @example {{{ tblMig.addColumns(_.col1, _.col2, _.column[Int]("fieldNotYetInTableDef")) }}}
   * @group oper
   */
  def addColumns(cols: (T => Rep[_])*) =
    modActions(withActions(cols.map(columnInfo andThen Action.AddColumn)))

  /**
   * @note `rawSqlExpr` is used as raw SQL, with the security implications thereof
   */
  def addColumnAndSetRaw(col: T => Rep[_], rawSqlExpr: String) =
    modActions(withActions(Action.AddColumnAndSetInitialValue(columnInfo(col), rawSqlExpr)))

  /**
   * Adds a column and populates it without setting a column default for the future.
   *
   * If the column is NOT NULL, it is first created nullable
   * @param col
   * @param value
   * @param jdbcType
   * @tparam C
   * @return
   */
  def addColumnAndSet[C](col: T => Rep[C], value: C)(implicit jdbcType: JdbcType[C]) =
    addColumnAndSetRaw(col, jdbcType.valueToSQLLiteral(value))

  /**
   * Drop columns.
   *
   * @param cols zero or more column-returning functions, which are passed the table object.
   * @example {{{ tblMig.dropColumns(_.col1, _.col2, _.column[Int]("oldFieldNotInTableDef")) }}}
   * @group oper
   */
  def dropColumns(cols: (T => Rep[_])*) =
    modActions(withActions(cols.map(columnInfo andThen Action.DropColumn)))

  def dropColumns(name: String, names: String*) =
    modActions(withActions((name +: names).map(Action.DropColumnOfName)))

  /**
   * Rename a column.
   *
   * @param col a column-returning function, which is passed the table object.
   * @example {{{ tblMig.renameColumn(_.col1, "newName") }}}
   * @group oper
   */
  def renameColumn(col: T => Rep[_], to: String) =
    modActions(withActions(Action.RenameColumnTo(columnInfo(col), to)))

  def renameColumnFrom(from: String, col: T => Rep[_]) =
    modActions(withActions(Action.RenameColumnFrom(columnInfo(col), from)))

  /**
   * Changes the data type of columns based on the column definitions in `cols`
   *
   * @param cols zero or more column-returning functions, which are passed the table object.
   * @example {{{ tblMig.alterColumnTypes(_.col1, _.column[NotTheTypeInTableDef]("col2")) }}}
   * @group oper
   */
  def alterColumnTypes(cols: (T => Rep[_])*) =
    modActions(withActions(cols.map(columnInfo andThen Action.AlterColumnType)))

  /**
   * Changes the default value of columns based on the column definitions in `cols`
   *
   * @param cols zero or more column-returning functions, which are passed the table object.
   * @example {{{ tblMig.alterColumnDefaults(_.col1, _.column[Int]("col2", O.Default("notTheDefaultInTableDef"))) }}}
   * @group oper
   */
  def alterColumnDefaults(cols: (T => Rep[_])*) =
    modActions(withActions(cols.map(columnInfo andThen Action.AlterColumnDefault)))

  /**
   * Changes the nullability of columns based on the column definitions in `cols`
   *
   * @param cols zero or more column-returning functions, which are passed the table object.
   * @example {{{ tblMig.alterColumnNulls(_.col1, _.column[Int]("col2", O.NotNull)) }}}
   * @group oper
   */
  def alterColumnNulls(cols: (T => Rep[_])*) =
    modActions(withActions(cols.map(columnInfo andThen Action.AlterColumnNullable)))

  private def pkInfo: (T => PrimaryKey) => PrimaryKeyInfo = { f =>
    val key = f(table)
    PrimaryKeyInfo(key.name, key.columns.flatMap(fieldSym))
  }

  /**
   * Adds primary key constraints.
   *
   * @param pks zero or more `PrimaryKey`-returning functions, which are passed the table object.
   * @example {{{ tblMig.addPrimaryKeys(_.pkDef) }}}
   * @group oper
   */
  def addPrimaryKeys(pks: (T => PrimaryKey)*) =
    modActions(withActions(pks.map(pkInfo andThen Action.AddPrimaryKey)))

  /**
   * Drops primary key constraints.
   *
   * @param pks zero or more `PrimaryKey`-returning functions, which are passed the table object.
   * @example {{{ tblMig.dropPrimaryKeys(_.pkDef) }}}
   * @group oper
   */
  def dropPrimaryKeys(pks: (T => PrimaryKey)*) =
    modActions(withActions(pks.map(pkInfo andThen Action.DropPrimaryKey)))

  /**
   * Adds foreign key constraints.
   *
   * @param fkqs zero or more `ForeignKeyQuery`-returning functions, which are passed the table object.
   * @example {{{ tblMig.addForeignKeys(_.fkDef) }}}
   * @group oper
   */
  def addForeignKeys(fkqs: (T => ForeignKeyQuery[_ <: AbstractTable[_], _])*) =
    modActions(withActions(fkqs.flatMap(_.apply(table).fks.map(Action.AddForeignKey))))

  /**
   * Drops foreign key constraints.
   *
   * @param fkqs zero or more `ForeignKeyQuery`-returning functions, which are passed the table object.
   * @example {{{ tblMig.dropForeignKeys(_.fkDef) }}}
   * @group oper
   */
  def dropForeignKeys(fkqs: (T => ForeignKeyQuery[_ <: AbstractTable[_], _])*) =
    modActions(withActions(fkqs.flatMap(_.apply(table).fks.map(Action.DropForeignKey))))

  /**
   * Adds indexes
   *
   * @param indexes zero or more `Index`-returning functions, which are passed the table object.
   * @example {{{ tblMig.addIndexes(_.idxDef) }}}
   * @group oper
   */
  def addIndexes(indexes: (T => Index)*) =
    modActions(withActions(indexes.map(i => Action.CreateIndex(indexInfo(i(table))))))

  /**
   * Drops indexes
   *
   * @param indexes zero or more `Index`-returning functions, which are passed the table object.
   * @example {{{ tblMig.dropIndexes(_.idxDef) }}}
   * @group oper
   */
  def dropIndexes(indexes: (T => Index)*) =
    modActions(withActions(indexes.map(i => Action.DropIndex(indexInfo(i(table))))))

  /**
   * Renames an index
   *
   * @param index an `Index`-returning function, which is passed the table object.
   * @example {{{ tblMig.renameIndex(_.idxDef, "newName") }}}
   * @group oper
   */
  def renameIndex(index: T => Index, to: String) =
    modActions(withActions(Action.RenameIndexTo(indexInfo(index(table)), to)))

  def renameIndexFrom(from: String, index: T => Index) =
    modActions(withActions(Action.RenameIndexFrom(indexInfo(index(table)), from)))
}
