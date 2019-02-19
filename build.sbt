crossScalaVersions := Seq("2.12.8", "2.11.12")

scalaVersion := "2.12.8"

organization := "io.github.nafg"

name := "slick-migration-api"

libraryDependencies += "com.typesafe.slick" %% "slick"                % "3.3.0"

libraryDependencies += "com.typesafe.slick" %% "slick-testkit"        % "3.3.0"     % "test"

libraryDependencies += "org.scalatest"      %% "scalatest"            % "3.0.5"     % "test"

libraryDependencies += "com.h2database"      % "h2"                   % "1.4.197"   % "test"

libraryDependencies += "org.xerial"          % "sqlite-jdbc"          % "3.25.2"    % "test" // latest version that works

// latest Derby that works
// 10.14.2.0 fails with java.security.AccessControlException: access denied org.apache.derby.security.SystemPermission( "engine", "usederbyinternals" )
libraryDependencies += "org.apache.derby"    % "derby"                % "10.11.1.1" % "test"

libraryDependencies += "org.hsqldb"          % "hsqldb"               % "2.4.1"     % "test"

libraryDependencies += "org.postgresql"      % "postgresql"           % "42.2.5"    % "test"

libraryDependencies += "mysql"               % "mysql-connector-java" % "8.0.15"    % "test"

libraryDependencies += "org.slf4j"           % "slf4j-simple"         % "1.7.26"    % "test"

scalacOptions in (Compile, doc) ++= Seq("-groups", "-implicits")

scalacOptions ++= Seq("-feature", "-deprecation")

testOptions in Test += Tests.Argument("-oF")
