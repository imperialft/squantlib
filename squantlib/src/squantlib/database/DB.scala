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

  /**
   * Returns a List of Country objects identified by a List of ID.
   * Usage: val countries = DB.getCountries(List("JPN", "KOR", "CHN"))
   *
   * @param ids A List of Country IDs.
   * @return A List of Country objects.
   */
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

  /**
   * Returns a List of InputParameters that falls onto a range of Dates.
   *
   * @param fromDate A starting point of Date. Range includes this date.
   *                   For example, when you specify this to be Jan 1st, 2012, look-up condition includes Jan 1st, 2012.
   *                   To be concise, the operator used for fromDate in where-clause is "greater than equal."
   * @param toDate A ending point of Date. Range does not include this date.
   *                 For example, when you specify this to be Jan, 2nd, 2012, look-up condition includes something like 2012-01-01 23:59:59.99999999 etc.
   *                 To be concise, the operator used for toDate in where-clause is "less than."
   * @param instrument An identifier of the instrument.
   * @param asset An identifier of the asset.
   * @param maturity An identifier of the maturity.
   * @return A List of matching InputParameters.
   */
  def getInputParameters(fromDate:JavaDate, toDate:JavaDate, instrument:String, asset:String, maturity:String):List[InputParameter] = {
    transaction {
      from(inputparameters)(ip =>
        where(
          (ip.paramdate  gte fromDate) and
          (ip.paramdate  lt  toDate) and
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

  /**
   * Returns a List of CDSParameters that falls onto a range of Dates.
   *
   * @param fromDate A starting point of Date. Range includes this date.
   *                   For example, when you specify this to be Jan 1st, 2012, look-up condition includes Jan 1st, 2012.
   *                   To be concise, the operator used for fromDate in where-clause is "greater than equal."
   * @param toDate A ending point of Date. Range does not include this date.
   *                 For example, when you specify this to be Jan, 2nd, 2012, look-up condition includes something like 2012-01-01 23:59:59.99999999 etc.
   *                 To be concise, the operator used for toDate in where-clause is "less than."
   * @param instrument An identifier of the instrument.
   * @param maturity An identifier of the maturity.
   * @param currencyid An identifier of the currency.
   * @param issuerid An identifier of the issuer.
   * @return A list of matching CDSParameters.
   */
  def getCDSParameters(fromDate:JavaDate, toDate:JavaDate, maturity:String, instrument:String, issuerid:String, currencyid:String):List[CDSParameter] = {
    transaction {
      from(cdsparameters)(cds =>
        where(
          (cds.paramdate  gte fromDate) and
          (cds.paramdate  lt  toDate) and
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
   * Inserts many Squeryl model objects via CSV.
   *
   * @param objects A List of Squeryl model objects.
   * @return Whether or not the statement ran successfully.
   *          However, this does not guarantee whether every row has been inserted.
   */
  def insertMany(objects:List[AnyRef]):Boolean = {
    buildCSVImportStatement(objects).foreach(runSQLStatement)

    true
  }

  /**
   * Runs a SQL statement.
   *
   * @param statement A SQL statement to run.
   * @return Whether or not the statement ran successfully.
   *          However, this does not guarantee whether every row has been inserted.
   */
  def runSQLStatement(statement:String):Boolean = {
    val session           = SessionFactory.concreteFactory.get()
    val preparedStatement = session.connection.prepareStatement(statement)
    preparedStatement.execute()
  }

  /**
   * Builds MySQL CSV import statement from multiple Squeryl objects.
   * CSV file for the objects will be automatically generated, and embedded in INFILE-clause in the query.
   *
   * @param objects List of a Squeryl objects of a same Model, such as List[BondPrice]
   * @return A List of strings of prepared SQL statement.
   */
  def buildCSVImportStatement(objects:List[AnyRef]):List[String] = {
    val tempFile            = File.createTempFile("squantlib", ".csv")
    tempFile.deleteOnExit()
    val tempFilePath        = tempFile.getAbsolutePath
    val tableNames          = tables.toList.map(t => t.posoMetaData.clasz.getSimpleName)
    val clazz               = objects(0).getClass
    val className           = clazz.getSimpleName.toString()
    val table               = tables(tableNames.indexOf(className))
    val tableName           = tables(tableNames.indexOf(className)).name
    val attrToField         = table.posoMetaData.fieldsMetaData.toList.map(fmd => (fmd.nameOfProperty, fmd.columnName))
    val builder             = new StringBuilder()
    val columnNames         = attrToField.map(pair => pair._2)
    builder.append(columnNames.mkString(",") + "\n")
    for (val obj <- objects) {
      builder.append(attrToField.map(pair => quoteValue(clazz.getMethod(pair._1).invoke(obj))).mkString(","))
      builder.append("\n")
    }
    val out = new BufferedWriter(new FileWriter(tempFile))
    out.write(builder.toString)
    out.close()

    List(
      "START TRANSACTION;",
      "SET FOREIGN_KEY_CHECKS = 0;",
      "LOAD DATA LOCAL INFILE '" + tempFilePath.replaceAll("\\\\", "\\\\\\\\") + "' " +
        "INTO TABLE " + tableName + " " +
        "FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '''' " +
        "IGNORE 1 LINES " +
        "(" + columnNames.mkString(", ") + ")" + ";",
      "COMMIT;"
    )
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
  def quoteTimestampWithTimezone(value:Timestamp):String = "'" + timeFormat.format(value) + "'"
  val timeFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SZ")

}