ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "constraints"
  )

libraryDependencies ++= Seq(
)

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-Yexplicit-nulls"
)