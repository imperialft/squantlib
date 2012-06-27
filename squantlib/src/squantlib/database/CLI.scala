// test

package squantlib.database

import java.io.{FileNotFoundException, FileInputStream}
import java.net.URLClassLoader
import java.util.Properties
import java.lang.Thread
import java.lang.management.{ManagementFactory, RuntimeMXBean}
import scala.tools.nsc.interpreter._
import scala.tools.nsc.Settings

object CLI {
  val properties = new Properties
  def getProperties() = properties

  def setup(propertiesPath:String):Unit = {
    try {
      properties.load(new FileInputStream(propertiesPath))
    } catch {
      case e:FileNotFoundException => {
        println("*.properties File does not exist: " + propertiesPath)
        System.exit(1)
      }
      case e:Exception => throw(e)
    }
    setupDb()
    println("DB = " + db.toString)
  }

  def main(args:Array[String]):Unit = {
    goInteractive(args)
  }

  /**
   * The DB accessor.
   *
   */
  var db:DB = null

  /**
   * Sets up the DB connection.
   *
   */
  private def setupDb():DB = {
    db = new DB(properties.getProperty("uri"), properties.getProperty("username"), properties.getProperty("password"))
    db
  }

  /**
   * Starts Scala's interactive command-line.
   *
   */
  private def goInteractive(args:Array[String]):Unit = {
    val interpreter = new InterpreterWrapper() {
      def prompt = "squantlib> "
      for (url <- Thread.currentThread.getContextClassLoader().asInstanceOf[URLClassLoader].getURLs)
          classPath(url.toString.replaceFirst("file:", ""))
      // autoRun("println(System.getProperty(\"java.class.path\"))")
      // BUG: Scala 2.9.2 seems to have problem inferring a runtime classpath.
      autoRun("squantlib.database.CLI.setup(\"" + args(0) + "\")")
      autoRun("cli = squantlib.database.CLI")
      autoImport("squantlib.database._")
    }
    interpreter.startInterpreting()
  }
}
