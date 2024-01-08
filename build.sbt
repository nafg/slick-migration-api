inThisBuild(List(
  crossScalaVersions := Seq("2.12.18", "2.13.12", "3.3.1"),
  scalaVersion := crossScalaVersions.value.last,
  organization := "io.github.nafg.slick-migration-api"
))

name := "slick-migration-api"

libraryDependencies += "com.typesafe.slick" %% "slick"                % "3.5.0-M5"

libraryDependencies += "com.typesafe.slick" %% "slick-testkit"        % "3.5.0-M5"     % "test"

libraryDependencies += "org.scalatest"      %% "scalatest"            % "3.2.17"     % "test"

libraryDependencies += "com.h2database"      % "h2"                   % "2.2.224"   % "test"

libraryDependencies += "org.xerial"          % "sqlite-jdbc"          % "3.44.1.0"    % "test" // latest version that works

libraryDependencies += "org.apache.derby"    % "derby"                % "10.15.2.0" % "test"
libraryDependencies += "org.apache.derby"    % "derbytools"           % "10.15.2.0" % "test"

libraryDependencies += "org.hsqldb"          % "hsqldb"               % "2.7.2"     % "test"

libraryDependencies += "org.postgresql"      % "postgresql"           % "42.7.1"    % "test"

libraryDependencies += "com.mysql"               % "mysql-connector-j" % "8.2.0"    % "test"

libraryDependencies += "org.slf4j"           % "slf4j-simple"         % "2.0.11"    % "test"

(Compile / doc / scalacOptions) ++= Seq("-groups", "-implicits")

scalacOptions ++= Seq("-feature", "-deprecation", "-Xsource:3")

Test / testOptions += Tests.Argument("-oF")
