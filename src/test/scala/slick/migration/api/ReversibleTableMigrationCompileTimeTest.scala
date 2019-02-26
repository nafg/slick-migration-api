package slick
package migration.api

import slick.jdbc.H2Profile

import org.scalatest.FunSuite


class ReversibleTableMigrationCompileTimeTest extends FunSuite {
  implicit val dialect: H2Dialect = new H2Dialect

  import H2Profile.api._


  val table1 = TableQuery[Table1]
  class Table1(tag: Tag) extends Table[(Int, Int)](tag, "table1") {
    def col1 = column[Int]("col1")
    def col2 = column[Int]("col2")
    def * = (col1, col2)
  }

  type Rev = TableMigration[Table1, TableMigration.Action.Reversible]
  type Irr = TableMigration[Table1, TableMigration.Action]

  // An empty TableMigration is reversible
  val tm = TableMigration(table1.baseTableRow)
  implicitly[tm.type <:< Rev]

  // A reversible operation on a reversible TableMigration: still reversible
  val tm1 = tm.create.addColumns(_.col1)
  implicitly[tm1.type <:< Rev]

  test("non-Reversible TableMigration does not become Reversible") {
    // Adding an irreversible operation results in an irreversible TableMigration
    val tm2 = tm1.drop
    implicitly[tm2.type <:< Irr]

    assertDoesNotCompile("implicitly[tm2.type <:< Rev]")

    // A reversible operation on an irreversible TableMigration: still irreversible
    val tm3 = tm2.addColumns(_.col2)
    implicitly[tm3.type <:< Irr]

    assertDoesNotCompile("implicitly[tm3.type <:< Rev]")
  }
}
