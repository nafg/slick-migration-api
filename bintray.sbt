publishMavenStyle := true

publishTo := Some("bintray" at "https://api.bintray.com/maven/naftoligug/maven/slick-migration-api")

sys.env.get("BINTRAYKEY").toSeq map (key =>
  credentials += Credentials(
    "Bintray API Realm",
    "api.bintray.com",
    "naftoligug",
    key
  )
)
