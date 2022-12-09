ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "Functional Scala 2022"
  )

libraryDependencies ++= Seq(
)

scalacOptions ++= Seq(
//  "-Xfatal-warnings"
)