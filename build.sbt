crossScalaVersions := Seq("2.12.1", "2.11.8")

scalaVersion := "2.12.1"

organization := "io.github.nafg"

name := "slick-migration-api"

libraryDependencies += "com.typesafe.slick" %% "slick"                % "3.2.0-M2"

libraryDependencies += "com.typesafe.slick" %% "slick-testkit"        % "3.2.0-M2"  % "test"

libraryDependencies += "org.scalatest"      %% "scalatest"            % "3.0.1"     % "test"

libraryDependencies += "com.h2database"      % "h2"                   % "1.4.194"   % "test"

libraryDependencies += "org.xerial"          % "sqlite-jdbc"          % "3.14.2.1"  % "test"

libraryDependencies += "org.apache.derby"    % "derby"                % "10.11.1.1" % "test"

libraryDependencies += "org.hsqldb"          % "hsqldb"               % "2.3.4"     % "test"

libraryDependencies += "org.postgresql"      % "postgresql"           % "9.4.1212"  % "test"

libraryDependencies += "mysql"               % "mysql-connector-java" % "6.0.5"     % "test"

libraryDependencies += "org.slf4j"           % "slf4j-simple"         % "1.7.22"    % "test"

scalacOptions in (Compile, doc) ++= Seq("-groups", "-implicits")

scalacOptions ++= Seq("-feature", "-deprecation")

testOptions in Test += Tests.Argument("-oF")
