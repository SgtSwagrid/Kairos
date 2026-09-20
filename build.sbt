import sbtunidoc.BaseUnidocPlugin.autoImport.*
import sbtunidoc.ScalaUnidocPlugin
import scala.language.implicitConversions

ThisBuild / name         := "kairos"
ThisBuild / organization := "com.alecdorrington"
ThisBuild / version      := "0.1.1-SNAPSHOT"

lazy val server = Subprojects.server
lazy val client = Subprojects.client
lazy val common = Subprojects.common

lazy val `kairos`: Project = project
  .in(file("."))
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(
    server,
    client,
    Subprojects.commonJvm,
    Subprojects.commonJs,
  )
  .settings(
    Compile / run / skip := true,
    run                  := (server / Compile / run).evaluated,
    ScalaUnidoc / unidoc / scalacOptions ++=
      Seq("-project", "Kairos"),
  )
