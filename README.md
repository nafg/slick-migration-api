Write typesafe and typo-safe database migrations, using your existing Slick table definitions.

[![Build Status](https://travis-ci.org/nafg/slick-migration-api.svg?branch=master)](https://travis-ci.org/nafg/slick-migration-api)
[![Coverage Status](https://img.shields.io/coveralls/nafg/slick-migration-api.svg)](https://coveralls.io/r/nafg/slick-migration-api?branch=master)
[![Download](https://api.bintray.com/packages/naftoligug/maven/slick-migration-api/images/download.svg) ](https://bintray.com/naftoligug/maven/slick-migration-api/_latestVersion)

### Dependency

| Slick version | SBT dependency                                                   |
|---------------|------------------------------------------------------------------|
| 3.2.3         | `"io.github.nafg" %% "slick-migration-api"         % "0.4.4"`    |
| 3.2.1         | `"io.github.nafg" %% "slick-migration-api"         % "0.4.2"`    |
| 3.2.0         | `"io.github.nafg" %% "slick-migration-api"         % "0.4.0"`    |
| 3.1.1         | `"io.github.nafg" %% "slick-migration-api"         % "0.3.0"`    |
| 3.0.3         | `"io.github.nafg" %% "slick-migration-api_slick30" % "0.3.0"`    |
| 2.1.0         | `"io.github.nafg" %% "slick-migration-api"         % "0.1.1"`    |

Artifacts are deployed to bintray and synchronized to JCenter, so add `resolvers += Resolver.jcenterRepo` to your build.


### Example

````scala
implicit val dialect = new H2Dialect

val init =
  TableMigration(myTable)
    .create
    .addColumns(_.col1, _.col2)
    .addIndexes(_.index1)
    .renameColumn(_.col03, "col3")
val seed =
  SqlMigration("insert into myTable (col1, col2) values (10, 20)")
  
val migration = init & seed

db.run(migration())
````
