package slick
package migration.api

import java.sql.SQLException
import java.util.logging.{Level, Logger}

import scala.concurrent.ExecutionContext

import slick.jdbc.GetResult._
import slick.jdbc._

import com.typesafe.slick.testkit.util.{ExternalJdbcTestDB, InternalJdbcTestDB, JdbcTestDB, TestDB}

object Dialects {
  implicit def derby   : Dialect[DerbyProfile   ] = new DerbyDialect
  implicit def h2      : Dialect[H2Profile      ] = new H2Dialect
  implicit def sqlite  : Dialect[SQLiteProfile  ] = new SQLiteDialect
  implicit def hsqldb  : Dialect[HsqldbProfile  ] = new HsqldbDialect
  implicit def mysql   : Dialect[MySQLProfile   ] = new MySQLDialect
  implicit def postgres: Dialect[PostgresProfile] = new PostgresDialect
}

import slick.migration.api.Dialects._

trait DialectTestDB { this: JdbcTestDB =>
}

class H2TestDB(name: String) extends InternalJdbcTestDB(name) with DialectTestDB {
  override val profile = H2Profile
  val url = "jdbc:h2:mem:test1"
  val jdbcDriver = "org.h2.Driver"
  override def isPersistent = false
  override lazy val capabilities = profile.capabilities + TestDB.capabilities.plainSql
}

class H2Test extends DbTest(new H2TestDB("h2mem")) with CompleteDbTest {
  override def noActionReturns = slick.model.ForeignKeyAction.Restrict
  override def longJdbcType = java.sql.Types.INTEGER
}

class HsqldbTest extends DbTest(new HsqlDB("hsqldbmem") {
  val dbName = "test1"
  val url = "jdbc:hsqldb:mem:"+dbName+";user=SA;password=;shutdown=true"
  override def isPersistent = false
}) with CompleteDbTest {
  override val catalog = None
  override val schema = Some("PUBLIC")
}

class SqliteTest extends DbTest[SQLiteProfile](new SQLiteTestDB("jdbc:sqlite::memory:", "sqlitemem") {
  override def isPersistent = false
  override def isShared = false
}) {
  override def getTables(implicit session: JdbcBackend#Session) =
    super.getTables.filterNot(t =>
      t.name.name == "sqlite_sequence" ||
      t.name.name.startsWith("sqlite_autoindex_")
    )
  override def longJdbcType = java.sql.Types.INTEGER
}

class DerbyTest extends DbTest(new DerbyDB("derbymem") {
  val dbName = "test1"
  val url = s"jdbc:derby:memory:$dbName;create=true"
  override def cleanUpBefore() = {
    val dropUrl = s"jdbc:derby:memory:$dbName;drop=true"
    try {
      val db = profile.backend.Database.forURL(dropUrl, driver = jdbcDriver)
      await(db.run(SimpleJdbcAction(_.connection)))
    } catch {
      case e: SQLException =>
    }
  }
}) with CompleteDbTest {
  override val catalog = None
  override val schema = Some("APP")
}

class MySQLTest extends DbTest(new ExternalJdbcTestDB("mysql") {
  override val profile = MySQLProfile
  override lazy val capabilities = profile.capabilities + TestDB.capabilities.plainSql
}) with CompleteDbTest {
  override val catalog = None
  override def columnDefaultFormat(s: String) = s
  override def getTables(implicit session: JdbcBackend#Session) =
    super.getTables.filterNot(_.name.name == "sys_config")
}

class PostgresTest extends DbTest(new ExternalJdbcTestDB("postgres") {
  override val profile = PostgresProfile
  override def localTables(implicit ec: ExecutionContext) = {
    val tables = ResultSetAction[(String,String,String, String)](_.conn.getMetaData.getTables("", "public", null, null))
    tables.map(_.filter(_._4.toUpperCase == "TABLE").map(_._3).sorted)
  }
  override def localSequences(implicit ec: ExecutionContext) =
    ResultSetAction[(String, String, String, String)](_.conn.getMetaData.getTables("", "public", null, null)).map { ts =>
      ts.filter(_._4.toUpperCase == "SEQUENCE").map(_._3).sorted
    }
  override lazy val capabilities = profile.capabilities + TestDB.capabilities.plainSql
}) with CompleteDbTest {
  override val schema = Some("public")
  override def longJdbcType = java.sql.Types.INTEGER
  override def columnDefaultFormat(s: String) = s"'$s'::character varying"
}


// copied from slick-testkit

abstract class HsqlDB(confName: String) extends InternalJdbcTestDB(confName) {
  override val profile = HsqldbProfile
  val jdbcDriver = "org.hsqldb.jdbcDriver"
  override def localTables(implicit ec: ExecutionContext) = {
    val tables = ResultSetAction[(String,String,String)](_.conn.getMetaData.getTables(null, "PUBLIC", null, null))
    tables.map(_.map(_._3).sorted)
  }
  override def cleanUpBefore(): Unit = {
    // Try to turn Hsqldb logging off -- does not work :(
    System.setProperty("hsqldb.reconfig_logging", "false")
    Logger.getLogger("org.hsqldb.persist.Logger").setLevel(Level.OFF)
    Logger.getLogger("org.hsqldb").setLevel(Level.OFF)
    Logger.getLogger("hsqldb").setLevel(Level.OFF)
  }
  override lazy val capabilities = profile.capabilities + TestDB.capabilities.plainSql
}

class SQLiteTestDB(dburl: String, confName: String) extends InternalJdbcTestDB(confName) {
  override val profile = SQLiteProfile
  val url = dburl
  val jdbcDriver = "org.sqlite.JDBC"
  override def localTables(implicit ec: ExecutionContext) =
    super.localTables.map(_.filter(s => !s.toLowerCase.contains("sqlite_")))
  override lazy val capabilities = profile.capabilities + TestDB.capabilities.plainSql
}

abstract class DerbyDB(confName: String) extends InternalJdbcTestDB(confName) {
  override val profile = DerbyProfile
  System.setProperty("derby.stream.error.method", classOf[DerbyDB].getName + ".DEV_NULL")
  val jdbcDriver = "org.apache.derby.jdbc.EmbeddedDriver"
  override def localTables(implicit ec: ExecutionContext) = {
    val tables = ResultSetAction[(String,String,String)](_.conn.getMetaData.getTables(null, "APP", null, null))
    tables.map(_.map(_._3).sorted)
  }
  override lazy val capabilities = profile.capabilities + TestDB.capabilities.plainSql
}

object DerbyDB {
  val DEV_NULL = new java.io.OutputStream { def write(b: Int): Unit = () }
}
