import AssemblyKeys._

assemblySettings

jarName in assembly := "squantlib.jar"

test in assembly := {}

name := "squantlib"

organization := "net.squantlib"

version := "0.1.0"

scalaVersion := "2.10.5"

libraryDependencies ++= Seq(
  "com.mchange" % "c3p0" % "0.9.2.1" % "provided",
  "org.squeryl" % "squeryl_2.9.2" % "0.9.6-RC2" % "provided",
  "org.apache.commons" % "commons-lang3" % "3.1" % "provided",
  "org.apache.commons" % "commons-math3" % "3.0" % "provided",
  "com.google.gdata" % "core" % "1.47.1" % "provided",
  "org.codehaus.jackson" % "jackson-core-asl" % "1.9.10" % "provided",
  "org.codehaus.jackson" % "jackson-mapper-asl" % "1.9.10" % "provided",
  "com.mchange" % "mchange-commons-java" % "0.2.3.3" % "provided",
  "org.jfree" % "jcommon" % "1.0.17" % "provided",
  "org.jfree" % "jfreechart" % "1.0.14" % "provided",
  "mysql" % "mysql-connector-java" % "5.1.10" % "provided",
  "com.typesafe" % "scalalogging-slf4j_2.10" % "1.1.0" % "provided",
  "org.slf4j" % "slf4j-api" % "1.7.5" % "provided",
  "ch.qos.logback" % "logback-classic" % "1.0.7" % "provided"
)

EclipseKeys.skipParents := false

classDirectory in Compile <<= baseDirectory apply ( _ / "target" / "classes" )

initialCommands := "import net.squantlib._"

retrieveManaged := false
