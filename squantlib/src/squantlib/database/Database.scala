package squantlib.database

import scala.collection.JavaConversions._
import java.sql._
import org.squeryl.adapters._
import org.squeryl.{Session,SessionFactory}

object SQL {
  def config(name:String):String = Core.databaseConfig.getProperty(name)
  var uri:String = null
  override def toString():String = uri
  config("adapter") match {
    case "h2" => {
      uri = "jdbc:h2:" + config("database")
      Class.forName("org.h2.Driver")
      SessionFactory.concreteFactory = Some(
        () => Session.create(DriverManager.getConnection(uri), new H2Adapter)
      )      
    }
    case "sqlite" => {
      uri = "jdbc:sqlite:" + config("database")
      Class.forName("org.sqlite.JDBC")
      SessionFactory.concreteFactory = Some(
        () => Session.create(DriverManager.getConnection(uri), new SQLiteAdapter)
      )      
    }
    case "mysql" => {
      uri = "jdbc:mysql://" + config("host") + ":" + config("port") + "/" + config("database")
      Class.forName("com.mysql.jdbc.Driver")
      SessionFactory.concreteFactory = Some(
        () => Session.create(DriverManager.getConnection(uri, config("username"), config("password")), new MySQLInnoDBAdapter)
      )
    }
  }
}

import java.util.{Date}
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Schema
import org.squeryl.annotations.Column

class Country(var id:         String,
              @Column("currency_id")
              var currencyId: String,
              var name:       String,
              @Column("short_name")
              var shortName:  Option[String],
              var region:     String) {
  override def toString():String = "Country<ID=" + id + ", Name=" + name + ">"
}

class Currency(var id: String,
               var name: String,
               @Column("short_name")
               var shortName: Option[String]) {
  override def toString():String = "Currency<ID=" + id + ">"
}

class Forex(@Column("currency_id")
            var currencyId: String,
            @Column("closed_on")
            var closedOn: Date,
            var rate: Float,
            @Column("updated_at")
            var updatedAt: Option[Timestamp]) {
  def this() = this("", new Date, 0.0F, Some(new Timestamp(0)))
  override def toString():String = "Forex<Rate=" + rate + ", currencyId=" + currencyId + ", closedOn=" + closedOn + ", updatedAt=" + updatedAt + ">"
}

object Database extends Schema {
  val countries = table[Country]("countries")
  val currencies = table[Currency]("currencies")
  val forexes = table[Forex]("forexes")

  def main(args:scala.Array[String]) : Unit = {
    countries.foreach(x => println(x.id + " " + x.currencyId + " " + x.name + " " + x.shortName + " " + x.region))
    }

}