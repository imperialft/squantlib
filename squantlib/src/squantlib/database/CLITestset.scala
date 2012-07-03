package squantlib.database

import java.io.{FileNotFoundException, FileInputStream}
import java.util.Properties

import org.apache.commons.lang3.StringEscapeUtils
import java.net.URLClassLoader
import java.lang.management.{ManagementFactory, RuntimeMXBean}
import scala.tools.nsc.{Interpreter, Settings}

object CLITestset {
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
    intp.interpret("import squantlib.database.objectconstructor._")
    intp.interpret("import squantlib.model.discountcurve._")
    intp.interpret("import org.jquantlib.time._")
    intp.interpret("import org.squeryl.PrimitiveTypeMode._")
    intp.interpret("CLI.setup(\"" + StringEscapeUtils.escapeJava(propertiesPath) + "\")")
    intp.interpret("DB.setup(CLI.properties.getProperty(\"uri\"), CLI.properties.getProperty(\"username\"), CLI.properties.getProperty(\"password\"))")
    println("Type 'exit [enter]' to quit.")

    // for example, this should work
//    intp.interpret("transaction { from(DB.countries)(c => select(c)).foreach(println) }")
//    intp.interpret("transaction { from(DB.currencies)(c => select(c)).foreach(println) }")
//    intp.interpret("transaction { from(DB.distributors)(c => select(c)).foreach(println) }")
//    intp.interpret("transaction { from(DB.issuers)(c => select(c)).foreach(println) }")
//    intp.interpret("transaction { from(DB.products)(c => select(c)).foreach(println) }")
//    intp.interpret("transaction { from(DB.bonds)(c => select(c)).foreach(println) }")

    intp.interpret("val vd = new Date(5, 5, 2012)")
    intp.interpret("val params = transaction { from(DB.inputparameters)(c => where(c.paramset === \"20120615-000\") select(c)).toSet }")
    intp.interpret("val ratecurves = LiborDiscountCurveConstructor.getcurves(params)")
    intp.interpret("val rateccycurve = ratecurves.map{case ((k1, k2), v) => (k1, v)} toMap")
    intp.interpret("val fxcurves = FXDiscountCurveConstructor.getcurves(params)")
    intp.interpret("val fxccycurve = fxcurves.map{case ((k1, k2), v) => (k1, v)} toMap")
    intp.interpret("val factory = new DiscountCurveFactory(rateccycurve ++ fxccycurve)")
    
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
