import AssemblyKeys._

assemblySettings

jarName in assembly := "squantlib.jar"

test in assembly := {}

name := "squantlib"

organization := "net.squantlib"

version := "0.1.0"

//scalaVersion := "2.10.5"
scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
//"com.mchange" % "c3p0" % "0.9.2.1" % "provided",
"com.mchange" % "c3p0" % "0.9.5.2" % "provided",
//"org.squeryl" %% "squeryl" % "0.9.6-RC1",
"org.squeryl" %% "squeryl" % "0.9.9",
//"org.apache.commons" % "commons-lang3" % "3.1" % "provided",
"org.apache.commons" % "commons-lang3" % "3.7" % "provided",
"org.apache.commons" % "commons-math3" % "3.6.1",
"com.google.gdata" % "core" % "1.47.1" % "provided",
"org.codehaus.jackson" % "jackson-core-asl" % "1.9.10" % "provided",
"org.codehaus.jackson" % "jackson-mapper-asl" % "1.9.10" % "provided",
"com.mchange" % "mchange-commons-java" % "0.2.3.3" % "provided",
"org.jfree" % "jcommon" % "1.0.24" % "provided",
"org.jfree" % "jfreechart" % "1.0.14" % "provided",
//"mysql" % "mysql-connector-java" % "5.1.10" % "provided",
"mysql" % "mysql-connector-java" % "6.0.6" % "provided",
//"com.typesafe" % "scalalogging-slf4j_2.10" % "1.1.0" % "provided",
//"com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2" % "provided",
"com.typesafe.scala-logging" %% "scala-logging" % "3.7.2" % "provided",
//"org.slf4j" % "slf4j-api" % "1.7.5" % "provided",
"org.slf4j" % "slf4j-api" % "1.7.25" % "provided",
//"ch.qos.logback" % "logback-classic" % "1.0.7" % "provided"
"ch.qos.logback" % "logback-classic" % "1.2.3" % "provided"
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

EclipseKeys.skipParents := false

// classDirectory in Compile <<= baseDirectory apply ( _ / "target" / "classes" )

classDirectory in Compile := baseDirectory.value / "target" / "classes"

initialCommands := "import net.squantlib._"

retrieveManaged := false