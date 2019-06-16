Write typesafe and typo-safe database migrations, using your existing Slick table definitions.

[![Build Status](https://travis-ci.org/nafg/slick-migration-api.svg?branch=master)](https://travis-ci.org/nafg/slick-migration-api)
[![Coverage Status](https://img.shields.io/coveralls/nafg/slick-migration-api.svg)](https://coveralls.io/r/nafg/slick-migration-api?branch=master)
[![Download](https://api.bintray.com/packages/naftoligug/maven/slick-migration-api/images/download.svg) ](https://bintray.com/naftoligug/maven/slick-migration-api/_latestVersion)

### Dependency

| Slick version | SBT dependency                                                   | Supported scala versions
|---------------|------------------------------------------------------------------|--------------------------
| 3.3.2         | `"io.github.nafg" %% "slick-migration-api"         % "0.7.0"`    | 2.12, 2.13
| 3.3.0         | `"io.github.nafg" %% "slick-migration-api"         % "0.6.1"`    | 2.11, 2.12
| 3.2.3         | `"io.github.nafg" %% "slick-migration-api"         % "0.4.4"`    | 2.11, 2.12
| 3.2.1         | `"io.github.nafg" %% "slick-migration-api"         % "0.4.2"`    | 2.11, 2.12
| 3.2.0         | `"io.github.nafg" %% "slick-migration-api"         % "0.4.0"`    | 2.11, 2.12
| 3.1.1         | `"io.github.nafg" %% "slick-migration-api"         % "0.3.0"`    | 2.11
| 3.0.3         | `"io.github.nafg" %% "slick-migration-api_slick30" % "0.3.0"`    | 2.10, 2.11
| 2.1.0         | `"io.github.nafg" %% "slick-migration-api"         % "0.1.1"`    | 2.10

Artifacts are deployed to bintray and synchronized to JCenter, so add `resolvers += Resolver.jcenterRepo` to your build.


### Example

````scala
import slick.jdbc.H2Profile.api._
import slick.migration.api._

val db = Database.forConfig("example-config")

implicit val dialect: H2Dialect = new H2Dialect

class MyTable(tag: Tag) extends Table[(Int, String)](tag, "my_table") {
  val col1 = column[Int]("col1")
  val col2 = column[String]("col2")
  val index1 = index("idx1", col1)
  def * = (col1, col2)
}
val MyTable = TableQuery[MyTable]
val init =
  TableMigration(MyTable)
    .create
    .addColumns(_.col1, _.col2)
    .addIndexes(_.index1)
    .renameColumn(_.col1, "col3")
val seed =
  SqlMigration("insert into myTable (col1, col2) values (10, 20)")
  
val migration = init & seed

db.run(migration())
````
