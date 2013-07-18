package scala.slick
package migration.api

import com.typesafe.slick.testkit.util.JdbcTestDB
import scala.slick.driver.H2Driver
import com.typesafe.slick.testkit.util.TestDB

class H2Test extends DbTest {
  object tdb extends JdbcTestDB("h2mem") {
    type Driver = H2Driver.type
    val driver = H2Driver
    val url = "jdbc:h2:mem:test1"
    val jdbcDriver = "org.h2.Driver"
    override def isPersistent = false
    override lazy val capabilities = driver.capabilities + TestDB.plainSql + TestDB.plainSqlWide
  }
}
