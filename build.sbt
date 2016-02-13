crossScalaVersions := Seq("2.11.6", "2.10.5")

organization := "io.github.nafg"

name := "slick-migration-api"

libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick"                % "3.0.3",
  "com.typesafe.slick" %% "slick-testkit"        % "3.0.3"         % "test",
  "org.scalatest"      %% "scalatest"            % "2.2.0"         % "test",
  "com.h2database"      % "h2"                   % "1.3.170"       % "test",
  "org.xerial"          % "sqlite-jdbc"          % "3.7.2"         % "test",
  "org.apache.derby"    % "derby"                % "10.10.1.1"     % "test",
  "org.hsqldb"          % "hsqldb"               % "2.2.8"         % "test",
  "org.postgresql"      % "postgresql"           % "9.3-1103-jdbc41" % "test",
  "mysql"               % "mysql-connector-java" % "5.1.31"        % "test",
  "org.slf4j"           % "slf4j-simple"         % "1.6.4"         % "test"
)

scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:postfixOps",
  "-language:implicitConversions",
  "-language:existentials",
  "-Xlint",
  "-Xfatal-warnings"
)

scalacOptions in (Compile, doc) ++= Seq("-groups", "-implicits")

testOptions in Test += Tests.Argument("-oF")
