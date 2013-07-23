scalaVersion := "2.10.1"

organization := "io.github.nafg"

name := "slick-migration-api"

libraryDependencies += "com.typesafe.slick"  % "slick_2.10.1"         % "2.0.0-M1"

libraryDependencies += "com.typesafe.slick"  % "slick-testkit_2.10.1" % "2.0.0-M1"      % "test"

libraryDependencies += "org.scalatest"      %% "scalatest"            % "2.0.M6-SNAP34" % "test"

libraryDependencies += "com.h2database"      % "h2"                   % "1.3.170"       % "test"

libraryDependencies += "org.xerial"          % "sqlite-jdbc"          % "3.7.2"         % "test"

libraryDependencies += "org.apache.derby"    % "derby"                % "10.9.1.0"      % "test"

libraryDependencies += "org.hsqldb"          % "hsqldb"               % "2.2.8"         % "test"

libraryDependencies += "postgresql"          % "postgresql"           % "9.1-901.jdbc4" % "test"

libraryDependencies += "mysql"               % "mysql-connector-java" % "5.1.23"        % "test"
