ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "fp-scala-exercises",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.12" % Test,
      "org.scalatestplus" %% "scalacheck-1-16" % "3.2.13.0" % Test
    ),
    Test / logBuffered := false
  )
