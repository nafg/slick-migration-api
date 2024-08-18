import _root_.io.github.nafg.mergify.dsl.*


mergifyExtraConditions := Seq(
  (Attr.Author :== "scala-steward") ||
    (Attr.Author :== "nafg-scala-steward[bot]")
)

inThisBuild(List(
  homepage := Some(url("https://github.com/nafg/slick-migration-api")),
  licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer("nafg", "Naftoli Gugenheim", "98384+nafg@users.noreply.github.com", url("https://github.com/nafg"))
  ),
  dynverGitDescribeOutput ~= (_.map(o => o.copy(dirtySuffix = sbtdynver.GitDirtySuffix("")))),
  dynverSonatypeSnapshots := true,
  githubWorkflowJavaVersions := Seq(JavaSpec.temurin("11")),
  githubWorkflowTargetTags ++= Seq("v*"),
  githubWorkflowBuild := Seq(
    WorkflowStep.Run(List("docker compose up -d mysql postgres"), name = Some("Start databases")),
    WorkflowStep.Sbt(List("-Dslick.testkit-config=test-dbs/testkit-github.conf", "test"), name = Some("Build project"))
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

sonatypeProfileName := "io.github.nafg"
