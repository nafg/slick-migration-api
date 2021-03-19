addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.1")
addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.2.7")
addSbtPlugin("com.geirsson" % "sbt-ci-release" % "1.5.6")
addSbtPlugin("com.codecommit" % "sbt-github-actions" % "0.10.1")
resolvers += "jitpack" at "https://jitpack.io"
libraryDependencies += "com.github.nafg" % "mergify-yaml" % "0.2.0"
