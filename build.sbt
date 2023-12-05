ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "AdventOfCode"
  )
libraryDependencies ++= Seq(
  "co.fs2" %% "fs2-core" % "3.9.3",
  "co.fs2" %% "fs2-io" % "3.9.3",
  "org.scalatest" %% "scalatest" % "3.2.15" % Test
)
