ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "AoC",
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-core" % "2.7.0",
      "com.lihaoyi" %% "requests" % "0.6.9",
    ),
    libraryDependencies ++= List(
      "org.scalatest" %% "scalatest" % "3.2.10"
    ).map(_ % Test)
  )
