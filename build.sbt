//import AssemblyKeys._

//assemblySettings

assemblyJarName in assembly := "squantlib.jar"

test in assembly := {}

name := "squantlib"

organization := "net.squantlib"

version := "0.1.0"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
"com.mchange" % "c3p0" % "0.9.2.1", // % "provided",
"org.squeryl" %% "squeryl" % "0.9.9",
"org.apache.commons" % "commons-lang3" % "3.7",
"org.apache.commons" % "commons-math3" % "3.6.1",
"com.google.gdata" % "core" % "1.47.1" % "provided",
"org.codehaus.jackson" % "jackson-core-asl" % "1.9.10" % "provided",
"org.codehaus.jackson" % "jackson-mapper-asl" % "1.9.10" % "provided",
"com.mchange" % "mchange-commons-java" % "0.2.3.4", // % "provided",
"org.jfree" % "jcommon" % "1.0.24" % "provided",
"org.jfree" % "jfreechart" % "1.0.14" % "provided",
"mysql" % "mysql-connector-java" % "5.1.30",
"com.typesafe.scala-logging" %% "scala-logging" % "3.5.0", // % "provided",
"ch.qos.logback" % "logback-classic" % "1.2.3" // % "provided"
)

scalacOptions ++= Seq(
  "-deprecation",
//  "-feature",
  //  "-unchecked",
  //  "-Xlint",
  "-Ywarn-dead-code"
//  "-Xlog-implicits"
  //"-Ywarn-numeric-widen",
  //"-Ywarn-value-discard"
)

classDirectory in Compile := baseDirectory.value / "target" / "classes"

initialCommands := "import net.squantlib._"

retrieveManaged := false

lazy val jquantlib = RootProject(file("../jquantlib"))

lazy val squantlib = Project(id = "squantlib", base = file(".")).dependsOn(jquantlib)
