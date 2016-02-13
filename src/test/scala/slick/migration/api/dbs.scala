package slick
package migration.api

import java.sql.SQLException
import java.util.logging.{ Level, Logger }

import slick.driver._
import slick.jdbc.{ StaticQuery => Q, JdbcBackend, ResultSetAction, ResultSetInvoker, SimpleJdbcAction }
import slick.jdbc.GetResult._

import com.typesafe.slick.testkit.util.JdbcTestDB
import com.typesafe.slick.testkit.util.InternalJdbcTestDB
import com.typesafe.slick.testkit.util.ExternalJdbcTestDB
import com.typesafe.slick.testkit.util.TestDB

import scala.concurrent.ExecutionContext

object Dialects {
  implicit def derby   : Dialect[DerbyDriver   ] = new DerbyDialect
  implicit def h2      : Dialect[H2Driver      ] = new H2Dialect
  implicit def sqlite  : Dialect[SQLiteDriver  ] = new SQLiteDialect
  implicit def hsqldb  : Dialect[HsqldbDriver  ] = new HsqldbDialect
  implicit def mysql   : Dialect[MySQLDriver   ] = new MySQLDialect
  implicit def postgres: Dialect[PostgresDriver] = new PostgresDialect
}

import Dialects._

trait DialectTestDB { this: JdbcTestDB =>
  type Drv <: JdbcDriver
}

class H2TestDB(name: String) extends InternalJdbcTestDB(name) with DialectTestDB {
  val driver = H2Driver
  val url = "jdbc:h2:mem:test1"
  val jdbcDriver = "org.h2.Driver"
  override def isPersistent = false
  override lazy val capabilities = driver.capabilities + TestDB.capabilities.plainSql
}

class H2Test extends DbTest(new H2TestDB("h2mem")) with CompleteDbTest {
  override def noActionReturns = slick.model.ForeignKeyAction.Restrict
}

class HsqldbTest extends DbTest(new HsqlDB("hsqldbmem") {
  val dbName = "test1"
  val url = "jdbc:hsqldb:mem:"+dbName+";user=SA;password=;shutdown=true"
  override def isPersistent = false
}) with CompleteDbTest {
  override val catalog = None
  override val schema = Some("PUBLIC")
}

class SqliteTest extends DbTest[SQLiteDriver](new SQLiteTestDB("jdbc:sqlite::memory:", "sqlitemem") {
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
  val driver = MySQLDriver
  override lazy val capabilities = driver.capabilities + TestDB.capabilities.plainSql
}) with CompleteDbTest {
  override def columnDefaultFormat(s: String) = s
}

class PostgresTest extends DbTest(new ExternalJdbcTestDB("postgres") {
  val driver = PostgresDriver
  override def localTables(implicit ec: ExecutionContext) = {
    val tables = ResultSetAction[(String,String,String, String)](_.conn.getMetaData.getTables("", "public", null, null))
    tables.map(_.filter(_._4.toUpperCase == "TABLE").map(_._3).sorted)
  }
  override def getLocalSequences(implicit session: profile.Backend#Session) = {
    val tables = ResultSetInvoker[(String,String,String, String)](_.conn.getMetaData.getTables("", "public", null, null))
    tables.buildColl[List].filter(_._4.toUpperCase == "SEQUENCE").map(_._3).sorted
  }
  override lazy val capabilities = driver.capabilities + TestDB.capabilities.plainSql
}) with CompleteDbTest {
  override val schema = Some("public")
  override def getTables(implicit session: JdbcBackend#Session) =
    super.getTables.filter(t =>
      t.tableType.toUpperCase == "TABLE"
    )
  override def longJdbcType = java.sql.Types.INTEGER
  override def columnDefaultFormat(s: String) = s"'$s'::character varying"
}


// copied from slick-testkit

abstract class HsqlDB(confName: String) extends InternalJdbcTestDB(confName) {
  val driver = HsqldbDriver
  val jdbcDriver = "org.hsqldb.jdbcDriver"
  override def localTables(implicit ec: ExecutionContext) = {
    val tables = ResultSetAction[(String,String,String)](_.conn.getMetaData.getTables(null, "PUBLIC", null, null))
    tables.map(_.map(_._3).sorted)
  }
  override def cleanUpBefore() {
    // Try to turn Hsqldb logging off -- does not work :(
    System.setProperty("hsqldb.reconfig_logging", "false")
    Logger.getLogger("org.hsqldb.persist.Logger").setLevel(Level.OFF)
    Logger.getLogger("org.hsqldb").setLevel(Level.OFF)
    Logger.getLogger("hsqldb").setLevel(Level.OFF)
  }
  override lazy val capabilities = driver.capabilities + TestDB.capabilities.plainSql
}

class SQLiteTestDB(dburl: String, confName: String) extends InternalJdbcTestDB(confName) {
  val driver = SQLiteDriver
  val url = dburl
  val jdbcDriver = "org.sqlite.JDBC"
  override def localTables(implicit ec: ExecutionContext) =
    super.localTables.map(_.filter(s => !s.toLowerCase.contains("sqlite_")))
  override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
    for(t <- getLocalTables)
      (Q.u+"drop table if exists "+driver.quoteIdentifier(t)).execute
    for(t <- getLocalSequences)
      (Q.u+"drop sequence if exists "+driver.quoteIdentifier(t)).execute
  }
  override lazy val capabilities = driver.capabilities + TestDB.capabilities.plainSql
}

abstract class DerbyDB(confName: String) extends InternalJdbcTestDB(confName) {
  val driver = DerbyDriver
  System.setProperty("derby.stream.error.method", classOf[DerbyDB].getName + ".DEV_NULL")
  val jdbcDriver = "org.apache.derby.jdbc.EmbeddedDriver"
  override def localTables(implicit ec: ExecutionContext) = {
    val tables = ResultSetAction[(String,String,String)](_.conn.getMetaData.getTables(null, "APP", null, null))
    tables.map(_.map(_._3).sorted)
  }
  override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
    try {
      try { (Q.u+"create table \"__derby_dummy\"(x integer primary key)").execute }
      catch { case ignore: SQLException => }
      val constraints = (Q[(String, String)]+"""
            select c.constraintname, t.tablename
            from sys.sysconstraints c, sys.sysschemas s, sys.systables t
            where c.schemaid = s.schemaid and c.tableid = t.tableid and s.schemaname = 'APP'
                                             """).buildColl[List]
      for((c, t) <- constraints if !c.startsWith("SQL"))
        (Q.u+"alter table "+driver.quoteIdentifier(t)+" drop constraint "+driver.quoteIdentifier(c)).execute
      for(t <- getLocalTables)
        (Q.u+"drop table "+driver.quoteIdentifier(t)).execute
      for(t <- getLocalSequences)
        (Q.u+"drop sequence "+driver.quoteIdentifier(t)).execute
    } catch {
      case e: Exception =>
        println("[Caught Exception while dropping user artifacts in Derby: "+e+"]")
        session.close()
        cleanUpBefore()
    }
  }
  override lazy val capabilities = driver.capabilities + TestDB.capabilities.plainSql
}

object DerbyDB {
  val DEV_NULL = new java.io.OutputStream { def write(b: Int) {} }
}
