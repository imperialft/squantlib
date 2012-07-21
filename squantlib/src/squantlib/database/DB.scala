package squantlib.database

import java.sql.{Timestamp, Time, DriverManager}
import com.mysql.jdbc.Driver
import com.mchange.v2.c3p0._
import org.squeryl.adapters._
import org.squeryl.{Session, SessionFactory, Schema}
import org.squeryl.PrimitiveTypeMode._
import squantlib.database.schemadefinitions._
import scala.collection.mutable.{MutableList, StringBuilder}
import org.jquantlib.time.{Date => JQuantDate}
import java.io.{File, FileWriter, BufferedWriter}
import java.util.{Date => JavaDate, Calendar => JavaCalendar, UUID}
import java.text.SimpleDateFormat
import org.apache.commons.lang3.StringEscapeUtils

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
  
  def getInputParamSets:Set[(String, JavaDate)] = {
    transaction {
        from(inputparameters)(p => 
          groupBy(p.paramset, p.paramdate))
          .map(q => (q.key._1, q.key._2)).toSet
    }
  }
  
  def getCDSParamSets:Set[(String, JavaDate)] = {
    transaction {
        from(cdsparameters)(p => 
          groupBy(p.paramset, p.paramdate))
          .map(q => (q.key._1, q.key._2)).toSet
    }
  }
  
  def checkParamSet(id:String):Boolean = {
    transaction {
      val inputcontains = from (inputparameters)(p => 
					        where(p.paramset === id) 
					        select(p)).size > 0
      val cdscontains = from (cdsparameters)(p => 
					        where(p.paramset === id) 
					        select(p)).size > 0
	  inputcontains && cdscontains
    }
  }
  
  def getParamSets:Set[(String, JavaDate)] = getInputParamSets & getCDSParamSets

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
  def buildCSVImportStatement(objects:List[AnyRef]):String = {
    val tempFile            = File.createTempFile("squantlib", ".csv")
    tempFile.deleteOnExit()
    val tempFilePath        = tempFile.getAbsolutePath
    val tableNames          = tables.toList.map(t => t.posoMetaData.clasz.getSimpleName)
    val clazz               = objects(0).getClass
    val className           = clazz.getSimpleName.toString()
    val table               = tables(tableNames.indexOf(className))
    val attrToField         = table.posoMetaData.fieldsMetaData.toList.map(fmd => (fmd.nameOfProperty, fmd.columnName))
    val builder             = new StringBuilder()
    builder.append(attrToField.map(pair => pair._2).mkString(", ") + "\n")
    for (val obj <- objects) {
      builder.append(attrToField.map(pair => quoteValue(clazz.getMethod(pair._1).invoke(obj))).mkString(", "))
      builder.append("\n")
    }
    val out = new BufferedWriter(new FileWriter(tempFile))
    out.write(builder.toString)
    out.close()

    "LOAD DATA INFILE \"" + tempFilePath + "\" INTO TABLE \"" + className + "\" FIELDS TERMINATED BY ',' ENCLOSED BY ''''"
  }

  /**
   * Converts a value into SQL (MySQL) representation. Throws java.lang.IllegalArgumentException if the value isn't supported.
   *
   * @param value Any object that is supported by Squeryl or JDBC.
   * @return A string of corresponding SQL representation.
   */
  def quoteValue(value:Any):String = {
    value match {
      case v:Number => quoteNumber(v)
      case v:String => quoteString(v)
      case v:Timestamp => quoteTimestamp(v)
      case v:JavaDate => quoteDate(v)
      case v:Boolean => quoteBoolean(v)
      case v:List[Any] => quoteList(v)
      case v:Some[Any] => quoteValue(v.get)
      case _ =>
        if (value == null || value == None)
          "NULL"
        else
          throw new java.lang.IllegalArgumentException("Cannot quote the value for SQL: " + value.toString)
    }
  }

  def quoteNumber(value:Number):String = value.toString
  def quoteString(value:String):String = "'" + value.replaceAll("'", "''") + "'"
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
  def quoteDate(value:JavaDate):String = quoteString(dateFormat.format(value))
  def quoteBoolean(value:Boolean):String = value.toString.toUpperCase

  /**
   * Converts a List of anything into SQL (MySQL) representation. For example, a List of Integer will be quoted into
   * something like (0, 1, 2, 3, 4) which you could use it for various statements like WHERE id IN ... etc.
   *
   * @param value A List of Anything that's supported by quoteValue() method.
   * @return A string of corresponding SQL representation.
   */
  def quoteList(value:List[Any]):String = "(" + value.map(quoteValue).mkString(",") + ")"

  /**
   * Converts a java.sql.Timestamp into SQL (MySQL) representation without time-zone information.
   *
   * @param value a java.sql.Timestamp instance.
   * @return A string of corresponding SQL representation.
   */
  def quoteTimestamp(value:Timestamp):String = "'" + value.toString + "'"

  /**
   * Converts a java.sql.Timestamp into SQL (MySQL) representation with time-zone information.
   * Return value would be something like '2012-07-21T16:13:49.758313+09:00'
   *
   * @param value a java.sql.Timestamp instance.
   * @return A string of corresponding SQL representation.
   *
   * @deprecated Because it doesn't work properly yet.
   *
   */
  def quoteTimestampWithTimezone(value:Timestamp) = "'" + timeFormat.format(value) + "'"
  val timeFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SZ")

}