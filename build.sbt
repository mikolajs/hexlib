name := "hexlib"

val scala3Version = "3.2.0"

ThisBuild / scalaVersion := "3.2.0"
ThisBuild / organizationName := "eu.brosbit"

cancelable in Global := true

lazy val root = project.
  in(file("."))
  .settings(
    name 	    := "hexlib",
    organization    := "eu.brosbit",
    scalaVersion    := scala3Version,
    version := "0.2" ,
    libraryDependencies ++= Seq(
      "org.scalactic" % "scalactic_3" % "3.2.14",
      "org.scalatest" % "scalatest_3" % "3.2.14" % "test",
      "ch.qos.logback"    % "logback-classic"           % "1.2.3"
    )
  )
