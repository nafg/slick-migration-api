inThisBuild(List(
  crossScalaVersions := Seq("2.12.18", "2.13.12"),
  scalaVersion := crossScalaVersions.value.last,
  organization := "io.github.nafg.slick-migration-api"
))

name := "slick-migration-api"

libraryDependencies += "com.typesafe.slick" %% "slick"                % "3.4.1"

libraryDependencies += "com.typesafe.slick" %% "slick-testkit"        % "3.4.1"     % "test"

libraryDependencies += "org.scalatest"      %% "scalatest"            % "3.2.17"     % "test"

libraryDependencies += "com.h2database"      % "h2"                   % "2.2.224"   % "test"

libraryDependencies += "org.xerial"          % "sqlite-jdbc"          % "3.43.2.0"    % "test" // latest version that works

libraryDependencies += "org.apache.derby"    % "derby"                % "10.15.2.0" % "test"
libraryDependencies += "org.apache.derby"    % "derbytools"           % "10.15.2.0" % "test"

libraryDependencies += "org.hsqldb"          % "hsqldb"               % "2.7.2"     % "test"

libraryDependencies += "org.postgresql"      % "postgresql"           % "42.6.0"    % "test"

libraryDependencies += "com.mysql"               % "mysql-connector-j" % "8.1.0"    % "test"

libraryDependencies += "org.slf4j"           % "slf4j-simple"         % "2.0.9"    % "test"

(Compile / doc / scalacOptions) ++= Seq("-groups", "-implicits")

scalacOptions ++= Seq("-feature", "-deprecation")

Test / testOptions += Tests.Argument("-oF")
