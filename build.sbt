name := "My FP Scala"

organization := "org.invisibletech"

version := "0.0.1"

scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test" withSources() withJavadoc(),
  "org.scalacheck" % "scalacheck_2.11" % "1.12.3" % "test" withSources() withJavadoc()
)

initialCommands := ""

