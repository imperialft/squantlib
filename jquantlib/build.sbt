
name := "jquantlib"

organization := "org.jquantlib"

version := "0.1.0"

scalaVersion := "2.10.2"

javaSource in Compile <<= baseDirectory(_ / "jquantlib")

libraryDependencies ++= Seq(
  "net.jcip" % "jcip-annotations" % "1.0" % "provided",
  "org.slf4j" % "slf4j-api" % "1.7.5" % "provided",
  "org.jfree" % "jcommon" % "1.0.17" % "provided",
  "org.jfree" % "jfreechart" % "1.0.14" % "provided",
  "org.joda" % "joda-primitives" % "1.0" % "provided",
  "org.specs2" % "specs2_2.9.2" % "1.10" % "provided",
  "org.specs2" % "specs2-scalaz-core_2.9.2" % "6.0.1" % "provided"
)

initialCommands := "import org.jquantlib._"

