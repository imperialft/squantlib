package squantlib.database

import java.sql.{Timestamp, DriverManager}
import org.squeryl.adapters._
import org.squeryl.{Session, SessionFactory, Schema}
import org.squeryl.PrimitiveTypeMode._
import squantlib.database.schemadefinitions._
import java.util.{Date => JavaDate, Calendar => JavaCalendar, UUID}
import org.jquantlib.time.{Date => JQuantDate}
import scala.collection.mutable.MutableList
import com.mchange.v2.c3p0._
import com.mysql.jdbc.Driver
import java.io.File
import scala.collection.mutable.StringBuilder

object DB extends Schema {

  val dataSource = new ComboPooledDataSource

  /**
   * Sets up the DB connection for current thread.
   * 
   * @param uri A connection string such as mysql://your.mysql.server:3128/database_name
   * @param username A username to MySQL
   * @param password Password for the user above
   *
   */
  def setup(uri:String, username:String, password:String):Unit = {

    dataSource.setDriverClass("com.mysql.jdbc.Driver")
    dataSource.setJdbcUrl("jdbc:" + uri + "?characterEncoding=utf-8")
    dataSource.setUser(username)
    dataSource.setPassword(password)
    dataSource.setMinPoolSize(5)
    dataSource.setMaxPoolSize(10)
    SessionFactory.concreteFactory = Some(() => Session.create(dataSource.getConnection, new MySQLInnoDBAdapter))
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
  
  def getAllBonds:List[Bond] = {
    transaction {
      from(bonds)(bond =>
        select(bond)
      ).toList
    }
  }
  
  def getBondsByProducts(productids:List[String]):List[String] = {
    transaction {
      from(bonds)(b =>
        where(b.productid in productids)
        select( &(b.id) )
      ).toList
    }
  }

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

  def getInputParameters(fromDate:JavaDate, toDate:JavaDate, instrument:String, asset:String, maturity:String):List[InputParameter] = {
    transaction {
      from(inputparameters)(ip =>
        where(
          (ip.paramdate  gte fromDate) and
          (ip.paramdate  lte  toDate) and
          ip.instrument === instrument and
          ip.asset      === asset and
          ip.maturity   === maturity
        )
        select(ip)
      ).toList
    }
  }

  def getCDSParameters(on:JavaDate, maturity:String, instrument:String, issuerid:String, currencyid:String):List[CDSParameter] = {
    transaction {
      from(cdsparameters)(cds =>
        where(
          cds.paramdate  === on and
          cds.instrument === instrument and
          cds.issuerid   === issuerid and
          cds.currencyid === currencyid
        )
        select(cds)
      ).toList
    }
  }

  def getCDSParameters(on:JQuantDate, maturity:String, instrument:String, issuerid:String, currencyid:String):List[CDSParameter] = getCDSParameters(on, maturity, instrument, issuerid, currencyid)

  def getCDSParameters(fromDate:JavaDate, toDate:JavaDate, maturity:String, instrument:String, issuerid:String, currencyid:String):List[CDSParameter] = {
    transaction {
      from(cdsparameters)(cds =>
        where(
          (cds.paramdate  gte fromDate) and
          (cds.paramdate  lte toDate) and
          cds.maturity   === maturity and
          cds.instrument === instrument and
          cds.issuerid   === issuerid and
          cds.currencyid === currencyid
        )
        select(cds)
      ).toList
    }
  }

  def setBondPrice(prices:Iterable[BondPrice]){
    val idset = prices.map(b => b.id)
    transaction {
      DB.bondprices.deleteWhere(b => b.id in idset)
      DB.bondprices.insert(prices)
    }
  }

  /**
   * Inserts many objects via CSV import statement. This operation involves reflections.
   * Also, you need Squeryl >= 0.9.6 (Snapshot is fine.)
   *
   * @param objects List of objects of type T.
   */
  def insertMany(objects:List[AnyRef]) = {
    val tempFile            = File.createTempFile("squantlib", ".csv")
    tempFile.deleteOnExit()
    val tempFilePath        = tempFile.getAbsolutePath
    val tableNames          = tables.toList.map(t => t.posoMetaData.clasz.getSimpleName)
    val clazz               = objects(0).getClass
    val className           = clazz.getSimpleName
    val table               = tables(tableNames.indexOf(className))
    val attrToField         = table.posoMetaData.fieldsMetaData.toList.map(fmd => (fmd.nameOfProperty, fmd.columnName))
    val builder             = new StringBuilder()
    for (val obj <- objects) {
      println("method, field, scala value to string")
      for (val pair <- attrToField) {
        val value = clazz.getMethod(pair._1).invoke(obj)
        println(pair._1 + ", " + pair._2 + ", " + value.toString)
      }
      builder.append("\n")
    }
  }

  /**
   * This method takes any property value of a model object, turn it into a string, and then quote it to SQL-safe format.
   *
   * @param value A value from a property of a model object.
   * @return       An SQL-safe quoted string.
   */
  def quoteValue(value:Any):String = ""

  def quoteNumber(value:Number):String = value.toString
  def quoteDate(value:JavaDate):String = value.toString
  def quoteString(value:String):String = {
    if (value.contains(","))
      "\"" + value.replaceAll("\"", "\\\"") + "\""
    else
      value
  }
  //def quoteTimestamp(value:Timestamp)
  //def quoteByteArray(value:Array[Byte])
  //def quoteUUID(value:UUID) = quoteString(value.toString)

}
