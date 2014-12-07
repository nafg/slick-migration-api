[![Build Status](https://travis-ci.org/pussinboots/slick-migration-api.svg?branch=master)](https://travis-ci.org/nafg/slick-migration-api)
[![Coverage Status](https://img.shields.io/coveralls/pussinboots/slick-migration-api.svg)](https://coveralls.io/r/pussinboots/slick-migration-api?branch=master)


A library for defining database migrations, for use with Slick,
including a DSL to define type safe and typo safe table migrations
that are defined in terms of Slick table definitions.

Example:

````scala
implicit val dialect = new H2Dialect

val migrate =
  TableMigration(myTable)
    .create
    .addColumns(_.col1, _.col2)
    .addIndexes(_.index1)
    .renameColumn(_.col03, "col3") &
  SqlMigration("insert into myTable (col1, col2) values (10, 20)")

withSession { implicit session: Session =>
  migrate()
}
````
