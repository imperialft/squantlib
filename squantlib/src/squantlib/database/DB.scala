package squantlib.database

import java.sql.DriverManager
import org.squeryl.adapters._
import org.squeryl.{Session, SessionFactory}
import org.squeryl.Schema

import squantlib.database.schemadefinitions._

/**
 * DB class provides a database connection, schema definitions, and the query interface.
 * Each row in the DB corresponds to an object defined in FIXME
 * 
 * @param uri A connection string such as mysql://your.mysql.server:3128/database_name
 * @param username A username to MySQL
 * @param password Password for the user above
 *
 */
class DB(uri:String, username:String, password:String) extends Schema {
  Class.forName("com.mysql.jdbc.Driver")
  SessionFactory.concreteFactory = Some(() => Session.create(DriverManager.getConnection("jdbc:" + uri, username, password), new MySQLInnoDBAdapter))
  override def toString():String = "DB<@uri=" + uri + ">"

  /**
   * Attach schema definitions to the tables.
   *
   * Example: 
   *
   *   val countries = table[Country]["countries"]
   *                         ^^^^^^^   ^^^^^^^^^
   *    A class in SchemaDefinitions   Actual table name in the database.
   */
  val countries = table[Country]("countries")

}