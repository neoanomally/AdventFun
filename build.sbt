name := "AdventOfCode"

version := "0.1"

scalaVersion := "3.3.4"

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "0.7.26" % Test,
  "org.scalacheck" %% "scalacheck" % "1.15.4",
	"org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
)

testFrameworks += new TestFramework("munit.Framework")

cancelable in Global := true
