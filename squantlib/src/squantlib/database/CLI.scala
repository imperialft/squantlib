package squantlib.database

import java.nio.charset.UnmappableCharacterException
import java.io.{FileNotFoundException, FileInputStream, File}
import java.util.Properties
import org.apache.commons.lang3.StringEscapeUtils
import java.net.URLClassLoader
import scala.tools.nsc.{Interpreter, Settings}

object CLI {

  /**
   * The main() expects an Array of String where the element of index 0 is *.properties file containing runtime
   * configuration information. These below are properties currently we are using.
   *
   * uri      = database connection string for JDBC, such as mysql://yourmysql.com:3306/database_name
   * username = a login user name for the database.
   * password = the password for that login.
   *
   */
  def main(args:Array[String]):Unit = {
    setup(args(0))
    goInteractive(args(0))
  }

  val properties = new Properties
  def setup(propertiesPath:String):Unit = properties.load(new FileInputStream(propertiesPath))

  val requiredImports = List("java.io.File",
                                "squantlib.database._",
                                "squantlib.database.schemadefinitions._",
                                "squantlib.database.objectconstructor._",
                                "org.jquantlib.time._",
                                "org.squeryl.PrimitiveTypeMode._",
                                "java.util.{Date => JavaDate, Calendar => JavaCalendar, UUID}",
                                "squantlib.task.pricing._"
                                )

  /**
   * @return true if the running environment is any kind of Windows.
   */
  def isWindows:Boolean = System.getProperty("os.name").contains("Windows")

  /**
   * @return Current working directory.
   */
  def currentDirectory:String = (new File("")).getAbsolutePath

  /**
   * goInteractive() starts a REPL CLI shell. At this moment, it is so hacky that I need to refactor this.
   *
   * @param propertiesPath A path to *.properties file that contains runtime configuration information.
   *
   */
  private def goInteractive(propertiesPath:String):Unit = {
    val escapedPropertiesPath = StringEscapeUtils.escapeJava(propertiesPath)
    val settings = new Settings()
    var classPathBuilder = "."

    if (isWindows) {
      for (path <- Thread.currentThread.getContextClassLoader().asInstanceOf[URLClassLoader].getURLs) {
        val cleanPath = path.toString.replaceFirst("file:/", "").replace("%20", " ")
        classPathBuilder = classPathBuilder + ";" + cleanPath
      }
    } else {
      for (path <- Thread.currentThread.getContextClassLoader().asInstanceOf[URLClassLoader].getURLs) {
        classPathBuilder = classPathBuilder + ":" + path
      }
    }

    settings.classpath.value = classPathBuilder
    val intp = new Interpreter(settings)

    intp beQuietDuring {
      requiredImports.foreach(s => intp.interpret("import " + s))
      intp.interpret("DB.setup(\"" +
        properties.get("uri") + "\", \"" +
        properties.get("username") + "\", \"" +
        properties.getProperty("password") +"\")"
      )
    }

    printHelp()

    // The REPL.
    var continue = true
    var i = 0
    while (continue) {
      printf("squantlib> ")
      val line = readLine().trim
      if (line != null) {
        i += 1
        println("  [" + i +"] " + line)
        // Exit
        if (line == "exit") continue = false
        else
        // Run script
        if (line.startsWith("run ")) {
          val file = line.slice(4, line.length)
          try
            intp.interpret(getSourceContent(file))
          catch {
            case e: UnmappableCharacterException => println("Encoding issue found. Is it in UTF-8?")
            case e: FileNotFoundException => println("File '" + file + "' does not exist.")
          }
        } else intp.interpret(line)
      } else continue = false
    }        
  }

  /**
   * Prints help to console.
   */
  def printHelp() = {
    println("Current working directory = " + currentDirectory)
    println("Type 'run file.scala' to run external *.scala. (Source file has to be in UTF-8)")
    println("Type 'exit [enter]' to quit.")
  }

  /**
   * Reads entire content of the file as a String.
   * Expect exceptions such as UnmappableCharacterException, or FileNotFoundException.
   * @param path A path to the file.
   * @param encoding The encoding of the file, such as UTF-8.
   * @return Content of the file.
   */
  def getSourceContent(path:String, encoding:String):String = scala.io.Source.fromFile(path, encoding).mkString
  def getSourceContent(path:String):String = getSourceContent(path, "UTF-8")
}
