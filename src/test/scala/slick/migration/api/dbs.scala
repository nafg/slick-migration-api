package slick
package migration.api

import java.sql.SQLException
import java.util.logging.{Level, Logger}

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.{global => ec}
import slick.jdbc.GetResult._
import slick.jdbc._
import com.typesafe.slick.testkit.util.{ExternalJdbcTestDB, InternalJdbcTestDB, JdbcTestDB, TestDB}
import org.scalatest.Ignore
import slick.dbio.{DBIO, DBIOAction, Effect, NoStream}
import slick.jdbc.meta.{MColumn, MTable}
import slick.lifted.{AbstractTable, TableQuery}
import slick.model.ForeignKeyAction


object Dialects {
  implicit def derby   : Dialect[DerbyProfile   ] = new DerbyDialect
  implicit def h2      : Dialect[H2Profile      ] = new H2Dialect
  implicit def sqlite  : Dialect[SQLiteProfile  ] = new SQLiteDialect
  implicit def hsqldb  : Dialect[HsqldbProfile  ] = new HsqldbDialect
  implicit def mysql   : Dialect[MySQLProfile   ] = new MySQLDialect
  implicit def postgres: Dialect[PostgresProfile] = new PostgresDialect
  implicit def oracle  : Dialect[OracleProfile  ] = new OracleDialect
}

import slick.migration.api.Dialects._

trait DialectTestDB { this: JdbcTestDB =>
}

class H2TestDB(name: String) extends InternalJdbcTestDB(name) with DialectTestDB {
  override val profile: H2Profile.type = H2Profile
  val url = "jdbc:h2:mem:test1"
  val jdbcDriver = "org.h2.Driver"
  override def isPersistent = false
  override lazy val capabilities = profile.capabilities + TestDB.capabilities.plainSql
}

class H2Test extends DbTest(new H2TestDB("h2mem")) with CompleteDbTest {
  override val noActionReturns: ForeignKeyAction = slick.model.ForeignKeyAction.Restrict
  override val longJdbcType = java.sql.Types.INTEGER
}

class HsqldbTest extends DbTest(new HsqlDB("hsqldbmem") {
  val dbName = "test1"
  val url = "jdbc:hsqldb:mem:"+dbName+";user=SA;password=;shutdown=true"
  override def isPersistent = false
}) with CompleteDbTest {
  override val catalog: Option[String] = None
  override val schema: Option[String] = Some("PUBLIC")
}

class SqliteTest extends DbTest(new SQLiteTestDB("jdbc:sqlite::memory:", "sqlitemem") {
  override def isPersistent = false
  override def isShared = false
}) {
  override def getTables =
    super.getTables
      .map(_.filterNot(t => t.name.name == "sqlite_sequence" || t.name.name.startsWith("sqlite_autoindex_")))
  override val longJdbcType = java.sql.Types.INTEGER
}

class DerbyTest extends DbTest(new DerbyDB("derbymem") {
  val dbName = "test1"
  val url = s"jdbc:derby:memory:$dbName;create=true"
  override def cleanUpBefore(): Unit = {
    val dropUrl = s"jdbc:derby:memory:$dbName;drop=true"
    try {
      val db = profile.backend.Database.forURL(dropUrl, driver = jdbcDriver)
      await(db.run(SimpleJdbcAction(_.connection)))
    } catch {
      case _: SQLException =>
    }
  }
}) with CompleteDbTest {
  override val catalog: Option[String] = None
  override val schema: Option[String] = Some("APP")
}

class MySQLTest extends DbTest(new ExternalJdbcTestDB("mysql") {
  override val profile: MySQLProfile.type = MySQLProfile
  override lazy val capabilities = profile.capabilities + TestDB.capabilities.plainSql
}) with CompleteDbTest {
  override val noActionReturns: ForeignKeyAction = slick.model.ForeignKeyAction.Restrict
  override val catalog: Option[String] = Some(tdb.confString("testDB"))
  override def columnDefaultFormat(s: String) = s
  override def getTables =
    super.getTables.map(_.filterNot(_.name.name == "sys_config"))
}

class PostgresTest extends DbTest(new ExternalJdbcTestDB("postgres") {
  override val profile: PostgresProfile.type = PostgresProfile
  override def localTables(implicit ec: ExecutionContext): DBIO[Vector[String]] = {
    val tables = ResultSetAction[(String,String,String, String)](_.conn.getMetaData.getTables("", "public", null, null))
    tables.map(_.filter(_._4.toUpperCase == "TABLE").map(_._3).sorted)
  }
  override def localSequences(implicit ec: ExecutionContext): DBIO[Vector[String]] =
    ResultSetAction[(String, String, String, String)](_.conn.getMetaData.getTables("", "public", null, null)).map { ts =>
      ts.filter(_._4.toUpperCase == "SEQUENCE").map(_._3).sorted
    }
  override lazy val capabilities = profile.capabilities + TestDB.capabilities.plainSql
}) with CompleteDbTest {
  override val schema: Option[String] = Some("public")
  override def columnDefaultFormat(s: String) = s"'$s'::character varying"
}

// copied from slick-testkit

// To test on Oracle:
// * Install Oracle DB
//   - manually from https://www.oracle.com/technetwork/database/database-technologies/express-edition/downloads/index.html
//   - or using Docker image: oracleinanutshell/oracle-xe-11g
// * Correct connection config in '<project root>/test-dbs/testkit.conf' according to your DB config
// * Download Oracle JDBC driver from https://www.oracle.com/technetwork/database/application-development/jdbc/downloads/index.html
// and put it into '<project root>/lib' directory
// * Remove '@Ignore' below
// * Run 'sbt testOnly *OracleTest'
@Ignore
class OracleTest extends DbTest(new ExternalJdbcTestDB("oracle") {
  override val profile: OracleProfile.type = OracleProfile
  import profile.api.actionBasedSQLInterpolation

  override def canGetLocalTables = false
  override def capabilities =
    super.capabilities - TestDB.capabilities.jdbcMetaGetIndexInfo - TestDB.capabilities.transactionIsolation

  override def localTables(implicit ec: ExecutionContext): DBIO[Vector[String]] = {
    val tableNames = profile.defaultTables.map(_.map(_.name.name)).map(_.toVector)
    tableNames
  }

  override def localSequences(implicit ec: ExecutionContext): DBIO[Vector[String]] = {
    // user_sequences much quicker than going to metadata if you don't know the schema they are going to be in
    sql"select sequence_Name from user_sequences".as[String]
  }

  override def dropUserArtifacts(implicit session: profile.backend.Session) =
    blockingRunOnSession { implicit ec =>
      for {
        tables <- localTables
        sequences <- localSequences
        _ <- DBIO.seq(tables.map(t => sqlu"drop table #${profile.quoteIdentifier(t)} cascade constraints") ++
                      sequences.map(s => sqlu"drop sequence #${profile.quoteIdentifier(s)}"): _*)
      } yield ()
    }
}) with CompleteDbTest {
  override val longJdbcType: Int = java.sql.Types.DECIMAL
  override val dateJdbcType: Int = java.sql.Types.TIMESTAMP
  override val noActionReturns: ForeignKeyAction = slick.model.ForeignKeyAction.Cascade

  override val schema: Option[String] = Some("SLICKTEST") // from testkit.conf

  override def getTables: DBIOAction[Vector[MTable],NoStream,Effect.All] =
    MTable.getTables(Some(""), schema, None, Some(Seq("TABLE")))

  // fixes returning column default value with spaces at the end (driver issue?)
  override def getColumns[E <: AbstractTable[_]](table: TableQuery[E]): DBIOAction[Vector[MColumn],NoStream,Effect.All] =
    super.getColumns(table).map(_.map { c =>
      c.copy(columnDef = c.columnDef.map(_.trim))
    })
}

abstract class HsqlDB(confName: String) extends InternalJdbcTestDB(confName) {
  override val profile: HsqldbProfile.type = HsqldbProfile
  val jdbcDriver = "org.hsqldb.jdbcDriver"
  override def localTables(implicit ec: ExecutionContext): DBIO[Vector[String]] = {
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

class SQLiteTestDB(dbUrl: String, confName: String) extends InternalJdbcTestDB(confName) {
  override val profile: SQLiteProfile.type = SQLiteProfile
  val url = dbUrl
  val jdbcDriver = "org.sqlite.JDBC"
  override def localTables(implicit ec: ExecutionContext) =
    super.localTables.map(_.filter(s => !s.toLowerCase.contains("sqlite_")))
  override lazy val capabilities = profile.capabilities + TestDB.capabilities.plainSql
}

abstract class DerbyDB(confName: String) extends InternalJdbcTestDB(confName) {
  override val profile: DerbyProfile.type = DerbyProfile
  System.setProperty("derby.stream.error.method", classOf[DerbyDB].getName + ".DEV_NULL")
  val jdbcDriver = "org.apache.derby.jdbc.EmbeddedDriver"
  override def localTables(implicit ec: ExecutionContext): DBIO[Vector[String]] = {
    val tables = ResultSetAction[(String,String,String)](_.conn.getMetaData.getTables(null, "APP", null, null))
    tables.map(_.map(_._3).sorted)
  }
  override lazy val capabilities = profile.capabilities + TestDB.capabilities.plainSql
}

object DerbyDB {
  val DEV_NULL = new java.io.OutputStream { def write(b: Int): Unit = () }
}
