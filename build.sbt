inThisBuild(List(
  crossScalaVersions := Seq("2.12.19", "2.13.14", "3.4.2"),
  scalaVersion := crossScalaVersions.value.last,
  organization := "io.github.nafg.slick-migration-api"
))

name := "slick-migration-api"

libraryDependencies += "com.typesafe.slick" %% "slick"                % "3.5.1"

libraryDependencies += "com.typesafe.slick" %% "slick-testkit"        % "3.5.1"     % "test"

libraryDependencies += "org.scalatest"      %% "scalatest"            % "3.2.19"     % "test"

libraryDependencies += "com.h2database"      % "h2"                   % "2.3.232"   % "test"

libraryDependencies += "org.xerial"          % "sqlite-jdbc"          % "3.46.0.1"    % "test" // latest version that works

libraryDependencies += "org.apache.derby"    % "derby"                % "10.15.2.0" % "test"
libraryDependencies += "org.apache.derby"    % "derbytools"           % "10.15.2.0" % "test"

libraryDependencies += "org.hsqldb"          % "hsqldb"               % "2.7.3"     % "test"

libraryDependencies += "org.postgresql"      % "postgresql"           % "42.7.3"    % "test"

libraryDependencies += "com.mysql"               % "mysql-connector-j" % "9.0.0"    % "test"

libraryDependencies += "org.slf4j"           % "slf4j-simple"         % "2.0.16"    % "test"

(Compile / doc / scalacOptions) ++= Seq("-groups", "-implicits")

scalacOptions ++= Seq("-feature", "-deprecation", "-Xsource:3")

Test / testOptions += Tests.Argument("-oF")
