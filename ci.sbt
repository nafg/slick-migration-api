import _root_.io.github.nafg.mergify.dsl.*


mergifyExtraConditions := Seq(
  (Attr.Author :== "scala-steward") ||
    (Attr.Author :== "nafg-scala-steward[bot]")
)

inThisBuild(List(
  homepage := Some(uri("https://github.com/nafg/slick-migration-api")),
  licenses := List("Apache-2.0" -> uri("https://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer("nafg", "Naftoli Gugenheim", "98384+nafg@users.noreply.github.com", uri("https://github.com/nafg"))
  ),
  dynverGitDescribeOutput ~= (_.map(o => o.copy(dirtySuffix = sbtdynver.GitDirtySuffix("")))),
  dynverSonatypeSnapshots := true,
  githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17")),
  githubWorkflowScalaVersions := githubWorkflowScalaVersions.value.map {
    case v if v.startsWith("3.") => "3.x"
    case v                       => v.replaceFirst("\\d+$", "x")
  },
  githubWorkflowTargetTags ++= Seq("v*"),
  // Workflow-wide: the sbt server that runs the tests is started by an earlier step,
  // so a -D flag on the test step would not reach it
  githubWorkflowEnv += "SLICK_TESTKIT_CONFIG" -> "test-dbs/testkit-github.conf",
  githubWorkflowBuild := Seq(
    WorkflowStep.Run(List("docker compose up -d mysql postgres"), name = Some("Start databases")),
    WorkflowStep.Sbt(List("test"), name = Some("Build project"))
  ),
  githubWorkflowPublishTargetBranches := Seq(RefPredicate.StartsWith(Ref.Tag("v"))),
  githubWorkflowPublish := Seq(
    WorkflowStep.Sbt(
      List("ci-release"),
      env = Map(
        "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
        "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
        "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
        "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
      )
    )
  )
))
