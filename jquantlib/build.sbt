import AssemblyKeys._

assemblySettings

name := "jquantlib"

organization := "org.jquantlib"

version := "0.1.0"

scalaVersion := "2.10.2"

javaSource in Compile <<= baseDirectory(_ / "jquantlib" / "src")

scalaSource in Compile <<= baseDirectory(_ / "jquantlib" / "src")

libraryDependencies ++= Seq(
  "net.jcip" % "jcip-annotations" % "1.0" % "provided",
  "org.slf4j" % "slf4j-api" % "1.7.5" % "provided",
  "org.jfree" % "jcommon" % "1.0.17" % "provided",
  "org.jfree" % "jfreechart" % "1.0.14" % "provided",
  "org.joda" % "joda-primitives" % "1.0" % "provided"
)

jarName in assembly := "jquantlib.jar"

excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
  cp filter { _.data.getName == "scala-compiler.jar" }
}

classDirectory in Compile <<= baseDirectory apply ( _ / "target" / "classes" )

initialCommands := "import org.jquantlib._"

