import sbt._
import Keys._

object MyBuild extends Build {

   lazy val jquantlib = RootProject(uri("https://github.com/imperialft/jquantlib.git#master"))
//   lazy val jquantlib = RootProject(file("../jquantlib"))

    lazy val squantlib = Project(id = "squantlib", base = file(".")) dependsOn(jquantlib)


}
