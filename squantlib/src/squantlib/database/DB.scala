package squantlib.database

import java.sql.DriverManager
import org.squeryl.adapters._
import org.squeryl.{Session, SessionFactory, Schema}
import org.squeryl.PrimitiveTypeMode._
import squantlib.database.schemadefinitions._
import java.util.{Date => JavaDate, Calendar => JavaCalendar}
import org.jquantlib.time.{Date => JQuantDate}
import scala.collection.mutable.MutableList

object DB extends Schema {
  
  /**
   * Sets up the DB connection for current thread.
   * 
   * @param uri A connection string such as mysql://your.mysql.server:3128/database_name
   * @param username A username to MySQL
   * @param password Password for the user above
   *
   */
  def setup(uri:String, username:String, password:String):Unit = {
    Class.forName("com.mysql.jdbc.Driver")
    SessionFactory.concreteFactory = Some(() => Session.create(DriverManager.getConnection("jdbc:" + uri + "?characterEncoding=utf-8", username, password), new MySQLInnoDBAdapter))
  }

  /**
   * Attach schema definitions to the tables.
   *
   * Example: 
   *
   *   val countries = table[Country]["countries"]
   *                         ^^^^^^^   ^^^^^^^^^
   *    A class in SchemaDefinitions   Actual table name in the database.
   */
  val countries = table[Country]("Countries")
  val currencies = table[Currency]("Currencies")
  val distributors = table[Distributor]("Distributors")
  val issuers = table[Issuer]("Issuers")
  val products = table[Product]("Products")
  val bonds = table[Bond]("Bonds")
  val inputparameters = table[InputParameter]("InputParameters")
  val cdsparameters = table[CDSParameter]("CDSParameters")
  val bondprices = table[BondPrice]("BondPrices")
  
  // NOTE: DB.countries.lookup("JPN") would return Japan. Similarly, DB.{currencies,distributors}.lookup("ID") should return corresponding object.

  def getCountries(ids:List[String]):List[Country] = {
    transaction {
      from(countries)(country =>
        where(country.id in ids)
        select(country)
      ).toList
    }
  }
  
  def getCurrencies(ids:List[String]):List[Currency] = {
    transaction {
      from(currencies)(currency =>
        where(currency.id in ids)
        select(currency)
      ).toList
    }
  }

  def getDistributers(ids:List[String]):List[Distributor] = {
    transaction {
      from(distributors)(distributor =>
        where(distributor.id in ids)
        select(distributor)
      ).toList
    }
  }

  def getIssuers(ids:List[String]):List[Issuer] = {
    transaction {
      from(issuers)(issuer =>
        where(issuer.id in ids)
        select(issuer)
      ).toList
    }
  }

  def getProducts(ids:List[String]):List[Product] = {
    transaction {
      from(products)(product =>
        where(product.id in ids)
        select(product)
      ).toList
    }
  }

  def getBonds(ids:List[String]):List[Bond] = {
    transaction {
      from(bonds)(bond =>
        where(bond.id in ids)
        select(bond)
      ).toList
    }
  }

  /**
   * 
   * @param
   *   on: A string representation of a date such as "2012-07-05" (anything that SimpleDateFormat constructor can take)
   *   instrument: A string of that specifies an instrument
   *   asset: A string that specifies an asset
   *   maturity: A string that specifies the maturity.
   * @returns List of InputParameter where size >= 0
   */
  def getInputParameters(on:JavaDate, instrument:String, asset:String, maturity:String):List[InputParameter] = {
    transaction {
      from(inputparameters)(ip =>
        where(
          ip.paramdate  === on and
          ip.instrument === instrument and
          ip.asset      === asset and
          ip.maturity   === maturity
        )
        select(ip)
      ).toList
    }
  }

  def getInputParameters(on:JQuantDate, instrument:String, asset:String, maturity:String):List[InputParameter] = getInputParameters(on.longDate, instrument, asset, maturity)

  /*
    def getInputParameters(from:JavaDate, to:JavaDate, instrument:String, asset:String, maturity:String):List[InputParameter] = {
      transaction {
        from(inputparameters)(ip =>
          where(
            ip.paramdate  gte from and
            in.paramdate  lt  to and
            ip.instrument === instrument and
            ip.asset      === asset and
            ip.maturity   === maturity
          )
          select(ip)
        ).toList
      }
    }
  */

  def getCDSParameters(on:JavaDate, maturity:String, instrument:String, issuerid:String, currencyid:String):List[CDSParameter] = {
    transaction {
      from(cdsparameters)(cds =>
        where(
          cds.paramdate  === on and
          cds.maturity   === maturity and
          cds.instrument === instrument and
          cds.issuerid   === issuerid and
          cds.currencyid === currencyid
        )
        select(cds)
      ).toList
    }
  }

  def getCDSParameters(on:JQuantDate, maturity:String, instrument:String, issuerid:String, currencyid:String):List[CDSParameter] = getCDSParameters(on, maturity, instrument, issuerid, currencyid)

  /*
    def getCDSParameters(from:JavaDate, to:JavaDate, maturity:String, instrument:String, issuerid:String, currencyid:String):List[CDSParameter] = {
      transaction {
        from(cdsparameters)(cds =>
          where(
            cds.paramdate  in  generateDateRange(from, to) and
            cds.maturity   === maturity and
            cds.instrument === instrument and
            cds.issuerid   === issuerid and
            cds.currencyid === currencyid
          )
          select(cds)
        ).toList
      }
    }
  */
  
  def generateDateRange(from:JavaDate, to:JavaDate):List[JavaDate] = {
    val list = MutableList[JavaDate]()
    val calendar = JavaCalendar.getInstance()
    calendar.setTime(from)
    var cache = calendar.getTime()
    while (cache.getTime() < to.getTime()) {
      list += cache
      calendar.add(JavaCalendar.DATE, 1)
      cache = calendar.getTime()
    }
    return list.toList
  }

}
