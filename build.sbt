scalaVersion := "2.10.1"

organization := "io.github.nafg"

name := "slick-migration-api"

libraryDependencies += "com.typesafe.slick"  % "slick_2.10.1"     % "2.0.0-M1"

libraryDependencies += "com.typesafe.slick"  % "slick-testkit_2.10.1" % "2.0.0-M1" % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M6-SNAP5" % "test"

libraryDependencies += "com.h2database" % "h2" % "1.3.170" % "test"
