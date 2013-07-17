import sbt._
import Keys._

object MyBuild extends Build {

    lazy val jquantlib = Project(id = "jquantlib", base = file("jquantlib"))
    lazy val squantlib = Project(id = "squantlib", base = file(".")) dependsOn(jquantlib)


}
