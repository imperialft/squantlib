package squantlib.database

import java.io.{FileNotFoundException, FileInputStream}
import java.util.Properties
import scala.tools.nsc.interpreter._
import scala.tools.nsc.Settings

object CLI {
  val properties = new Properties
  def getProperties() = properties

  def main(args:Array[String]):Unit = {
    if (args.length <= 0) {
      println("Usage: scala -cp ... squantlib.database.CLI your_settings.properties")
    } else {
      try {
        properties.load(new FileInputStream(args(0)))
      } catch {
        case e:FileNotFoundException => {
          println("*.properties File does not exist: " + args(0))
          System.exit(1)
        }
        case e:Exception => throw(e)
      }
    }
    setupDb()
    println("DB = " + db.toString)
    goInteractive()
  } // def main()

  /**
   * The DB accessor.
   *
   */
  var db:DB = null
  def getDb() = db

  /**
   * Sets up the DB connection.
   *
   */
  private def setupDb():Unit = {
    db = new DB(properties.getProperty("uri"), properties.getProperty("username"), properties.getProperty("password"))
  }

  /**
   * Starts Scala's interactive command-line.
   *
   */
  private def goInteractive():Unit = {    
    val interpreter = new InterpreterWrapper() {
      def prompt = "SquantLib> "
      def welcomeMsg = "Current CLI object is available as 'cli'.\nFor example, try 'cli.db [enter]' to see the database connection.\nType ':help [enter]' for more help.\nPlease wait until the interpreter warms up...\n"
      def helpMsg = "Need help?"
      // Assigns 'this' CLI object to the value 'cli' in the interpreter's scope.
      bind("cli", this)
      // Makes interpreter import these Java packages automatically.
      // BUG: Scala 2.9.2 seems to have problem inferring a runtime classpath.
      autoImport("squantlib.database._")
    }
    interpreter.startInterpreting()
  }
}