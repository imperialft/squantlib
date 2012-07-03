package squantlib.database

import java.io.{FileNotFoundException, FileInputStream}
import java.util.Properties

import org.apache.commons.lang3.StringEscapeUtils
import java.net.URLClassLoader
import java.lang.management.{ManagementFactory, RuntimeMXBean}
import scala.tools.nsc.{Interpreter, Settings}

object CLI {
  val properties = new Properties
  def setup(propertiesPath:String):Unit = properties.load(new FileInputStream(propertiesPath))
  def main(args:Array[String]):Unit = goInteractive(args(0))

  /**
   * Starts Scala's interactive command-line.
   *
   */
  private def goInteractive(propertiesPath:String):Unit = {
    val settings = new Settings()
    var classpath = ""
    for (url <- Thread.currentThread.getContextClassLoader().asInstanceOf[URLClassLoader].getURLs)
      classpath = classpath + ";" + url.toString.replaceFirst("file:", "")
    settings.classpath.value = classpath
    val intp = new Interpreter(settings)

    intp.interpret("import squantlib.database._")
    intp.interpret("import squantlib.database.schemadefinitions._")
    intp.interpret("import squantlib.database.objectconstructor._")
    intp.interpret("import org.jquantlib.time._")
    intp.interpret("import org.squeryl.PrimitiveTypeMode._")
    intp.interpret("CLI.setup(\"" + StringEscapeUtils.escapeJava(propertiesPath) + "\")")
    intp.interpret("DB.setup(CLI.properties.getProperty(\"uri\"), CLI.properties.getProperty(\"username\"), CLI.properties.getProperty(\"password\"))")
    println("Type 'exit [enter]' to quit.")
    intp.interpret("import java.io.File")
    intp.interpret("println(\"CLI's current working directory is\" + (new File(\".\").getAbsolutePath()))")
    println("Type 'run file.scala' to run external *.scala.")

    // for example, this should work
    intp.interpret("transaction { from(DB.countries)(c => select(c)).foreach(println) }")
    
    var continue = true
    while (continue) {
      printf("squantlib> ")
      
      val line = readLine()
      if (line != null) {
        if (line == "exit") {
          continue = false
        } else if (line.startsWith("run ")) {
          val file = line.split(" ", 2)(1)
          try {
            intp.interpret(scala.io.Source.fromFile(file).mkString)
          } catch {
            case e:Exception => {
              println("Failed to load file '" + file + "'. Does it exist?")
            }
          }
        } else {
          intp.interpret(line)
        }
      } else {
        continue = false
      }
    }        
  }
}
