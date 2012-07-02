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
    // Import the default packages.
    intp.interpret("import squantlib.database._")
    intp.interpret("import squantlib.database.schemadefinitions._")
    intp.interpret("import org.squeryl.PrimitiveTypeMode._")
    intp.interpret("CLI.setup(\"" + StringEscapeUtils.escapeJava(propertiesPath) + "\")")
    intp.interpret("DB.setup(CLI.properties.getProperty(\"uri\"), CLI.properties.getProperty(\"username\"), CLI.properties.getProperty(\"password\"))")
    println("Type 'exit [enter]' to quit.")

    // for example, this should work
    intp.interpret("transaction { from(DB.countries)(c => select(c)).foreach(println) }")
    intp.interpret("transaction { from(DB.currencies)(c => select(c)).foreach(println) }")
    intp.interpret("transaction { from(DB.distributors)(c => select(c)).foreach(println) }")
    intp.interpret("transaction { from(DB.issuers)(c => select(c)).foreach(println) }")
    intp.interpret("transaction { from(DB.products)(c => select(c)).foreach(println) }")
    intp.interpret("transaction { from(DB.bonds)(c => select(c)).foreach(println) }")

// def cond(c:InputParameter) = c.paramset == "20120615-000"    
// val c = transaction { from(DB.inputparameters)(c => where(c.paramset === "20120615-000") select(c)).toList }
// val bz = transaction { from(DB.bonds)(c => select(c)).toList }
    
    var continue = true
    while (continue) {
      printf("squantlib> ")
      
      val line = readLine()
      if (line != null) {
        if (line == "exit")
          continue = false
        else
          intp.interpret(line)
      } else {
        continue = false
      }
    }        
  }
}
