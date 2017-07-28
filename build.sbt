crossScalaVersions := Seq("2.12.3", "2.11.11")

scalaVersion := "2.12.3"

organization := "io.github.nafg"

name := "slick-migration-api"

libraryDependencies += "com.typesafe.slick" %% "slick"                % "3.2.1"

libraryDependencies += "com.typesafe.slick" %% "slick-testkit"        % "3.2.1"     % "test"

libraryDependencies += "org.scalatest"      %% "scalatest"            % "3.0.3"     % "test"

libraryDependencies += "com.h2database"      % "h2"                   % "1.4.196"   % "test"

libraryDependencies += "org.xerial"          % "sqlite-jdbc"          % "3.14.2.1"  % "test" // latest version that works

libraryDependencies += "org.apache.derby"    % "derby"                % "10.11.1.1" % "test" // latest version that works

libraryDependencies += "org.hsqldb"          % "hsqldb"               % "2.4.0"     % "test"

libraryDependencies += "org.postgresql"      % "postgresql"           % "42.1.3"    % "test"

libraryDependencies += "mysql"               % "mysql-connector-java" % "6.0.6"     % "test"

libraryDependencies += "org.slf4j"           % "slf4j-simple"         % "1.7.25"    % "test"

scalacOptions in (Compile, doc) ++= Seq("-groups", "-implicits")

scalacOptions ++= Seq("-feature", "-deprecation")

testOptions in Test += Tests.Argument("-oF")
