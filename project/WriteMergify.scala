import io.github.nafg.mergify.dsl._

import sbt._
import sbtghactions.GenerativePlugin
import sbtghactions.GenerativePlugin.autoImport._


object WriteMergify extends AutoPlugin {
  override def requires = GenerativePlugin
  override def trigger = allRequirements
  override def projectSettings = Seq(
    githubWorkflowGenerate := {
      githubWorkflowGenerate.value
      for (job <- githubWorkflowGeneratedCI.value if job.id == "build")
        IO.write(
          file(".mergify.yml"),
          mergify
            .withRule("Automatically merge successful scala-steward PRs")(
              (Attr.Author :== "scala-steward") +:
                (for (o <- job.oses; s <- job.scalas; v <- job.javas) yield
                  Attr.CheckSuccess :== s"${job.name} ($o, $s, $v)"): _*
            )(Action.Merge(strict = true))
            .toYaml
        )
    }
  )
}
