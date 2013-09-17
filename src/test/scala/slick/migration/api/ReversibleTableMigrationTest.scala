package scala.slick
package migration.api

import scala.slick.driver.H2Driver

object ReversibleTableMigrationCompileTimeTest {
  object tms extends TableMigrations(H2Driver)
  import tms._

  import H2Driver.simple._

  object table1 extends Table[(Int, Int)]("table1") {
    def col1 = column[Int]("col1")
    def col2 = column[Int]("col2")
    def * = col1 ~ col2
  }

  type Rev = ReversibleTableMigration[table1.type]
  type Irr = IrreversibleTableMigration[table1.type]

  // An empty TableMigration is reversible
  val tm = TableMigration(table1)
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
