package squantlib.database

import java.io.FileInputStream
import java.util.Properties

object Core {
  def noop():Unit = {}
  val version:String = "0.1.0"
  val environment:String = scala.util.Properties.envOrElse("VERTEX_ENV", "development")
  println("Vertex Core (" + Core.version + ", " + Core.environment + ")")

  def getProperties(name:String):Properties = {
    val properties = new Properties()
    properties.load(new FileInputStream("config/" + environment + "/" + name + ".properties"))
    return properties
  }

  val databaseConfig:Properties = getProperties("database")
  println("SQL = " + SQL)
}