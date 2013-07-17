import AssemblyKeys._

assemblySettings

name := "squantlib"

organization := "net.squantlib"

version := "0.1.0"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "com.mchange" % "c3p0" % "0.9.2.1" % "provided",
  "org.squeryl" %% "squeryl" % "0.9.6-RC1" % "provided",
  "org.apache.commons" % "commons-lang3" % "3.1" % "provided",
  "org.apache.commons" % "commons-math3" % "3.0" % "provided",
  "com.google.gdata" % "core" % "1.47.1" % "provided",
  "org.codehaus.jackson" % "jackson-core-asl" % "1.9.10" % "provided",
  "org.codehaus.jackson" % "jackson-mapper-asl" % "1.9.10" % "provided",
  "com.mchange" % "mchange-commons-java" % "0.2.3.3" % "provided",
  "log4j" % "log4j" % "1.2.17" % "provided",
  "org.jfree" % "jcommon" % "1.0.17" % "provided",
  "org.jfree" % "jfreechart" % "1.0.14" % "provided",
  "mysql" % "mysql-connector-java" % "5.1.10" % "provided"
)

jarName in assembly := "squantlib.jar"

excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
  cp filter { _.data.getName == "scala-compiler.jar" }
}

initialCommands := "import net.squantlib._"

