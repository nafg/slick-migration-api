package slick
package migration.api

import driver.H2Driver

object ReversibleTableMigrationCompileTimeTest {
  implicit val dialect = new H2Dialect

  import H2Driver.api._

  val table1 = TableQuery[Table1]
  class Table1(tag: Tag) extends Table[(Int, Int)](tag, "table1") {
    def col1 = column[Int]("col1")
    def col2 = column[Int]("col2")
    def * = (col1, col2)
  }

  type Rev = ReversibleTableMigration[Table1]
  type Irr = IrreversibleTableMigration[Table1]

  // An empty TableMigration is reversible
  val tm = TableMigration(table1.baseTableRow)
  implicitly[tm.type <:< Rev]

  // A reversible operation on a reversible TableMigration: still reversible
  val tm1 = tm.create.addColumns(_.col1)
  implicitly[tm1.type <:< Rev]

  // Adding an irreversible operation results in an irreversible TableMigration
  val tm2 = tm1.drop
  implicitly[tm2.type <:< Irr]

  // A reversible operation on an irreversible TableMigration: still irreversible
  val tm3 = tm2.addColumns(_.col2)
  implicitly[tm3.type <:< Irr]
}
