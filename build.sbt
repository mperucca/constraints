ThisBuild / scalaVersion := "3.4.0"

lazy val root = (project in file("."))
  .settings(
    name := "constraints"
  )

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.16" % Test
)

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-Yexplicit-nulls",
  "-deprecation"
)