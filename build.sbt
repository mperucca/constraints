inThisBuild(List(
  organization := "io.github.mperucca",
  homepage := Some(url("https://github.com/mperucca/constraints")),
  licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer(
      "mperucca",
      "Michael Perucca",
      "michaelperucca@gmail.com",
      url("https://github.com/mperucca")
    )
  ),
  sonatypeCredentialHost := "s01.oss.sonatype.org",
  sonatypeRepository := "https://s01.oss.sonatype.org/service/local",
  scalaVersion := "3.3.4"
))

lazy val root = (project in file("."))
  .settings(
    name := "constraints"
  )

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.18" % Test
)

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-Yexplicit-nulls",
  "-deprecation"
)
