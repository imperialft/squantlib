package squantlib.database

import java.sql.{Timestamp, Time, DriverManager}
import com.mysql.jdbc.Driver
import com.mchange.v2.c3p0._
import org.squeryl.adapters._
import org.squeryl.{Session, SessionFactory, Schema}
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.{KeyedEntity, Table}
import squantlib.database.schemadefinitions._
import scala.collection.mutable.{MutableList, StringBuilder}
import scala.collection.immutable.TreeMap
import scala.collection.SortedMap
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
  val fxrates = table[FXRate]("FXRates")
  val issuers = table[Issuer]("Issuers")
  val products = table[Product]("Products")
  val bonds = table[Bond]("Bonds")
//  val inputparameters = table[InputParameter]("InputParameters")
  val ratefxparameters = table[RateFXParameter]("RateFXParameters")
  val cdsparameters = table[CDSParameter]("CDSParameters")
  val bondprices = table[BondPrice]("BondPrices")
  val volatilities = table[Volatility]("Volatilities")
  val correlations = table[Correlation]("Correlations")
  val coupons = table[Coupon]("Coupons")
  val forwardprices = table[ForwardPrice]("ForwardPrices")

  /**
   * Returns a List of Country objects identified by a List of ID.
   * Usage: val countries = DB.getCountries(List("JPN", "KOR", "CHN"))
   *
   * @param ids A List of Country IDs.
   * @return A List of Country objects.
   */
  def getCountries:Set[Country] = {
    transaction {
      from(countries)(country => select(country)).toSet
    }
  }
  
  def getCountries(ids:Traversable[String]):Set[Country] = {
    transaction {
      from(countries)(country =>
        where(country.id in ids)
        select(country)
      ).toSet
    }
  }

  def getCurrencies:Set[Currency] = {
    transaction {
      from(currencies)(currency => select(currency)).toSet
    }
  }
  
  def getCurrencies(ids:Traversable[String]):Set[Currency] = {
    transaction {
      from(currencies)(currency =>
        where(currency.id in ids)
        select(currency)
      ).toSet
    }
  }

  def getDistributers:Set[Distributor] = {
    transaction {
      from(distributors)(distributor => select(distributor)).toSet
    }
  }
  
  def getDistributers(ids:Traversable[String]):Set[Distributor] = {
    transaction {
      from(distributors)(distributor =>
        where(distributor.id in ids)
        select(distributor)
      ).toSet
    }
  }

  def getIssuers:Set[Issuer] = {
    transaction {
      from(issuers)(issuer => select(issuer)).toSet
    }
  }

  def getIssuers(ids:Traversable[String]):Set[Issuer] = {
    transaction {
      from(issuers)(issuer =>
        where(issuer.id in ids)
        select(issuer)
      ).toSet
    }
  }
  
  def getProducts:Set[Product] = {
    transaction {
      from(products)(product => select(product)).toSet
    }
  }
  
  def getProducts(ids:Traversable[String]):Set[Product] = {
    transaction {
      from(products)(product =>
        where(product.id in ids)
        select(product)
      ).toSet
    }
  }
  
  def getBonds:Set[Bond] = {
    transaction {
      from(bonds)(bond => select(bond)).toSet
    }
  }
  
  def getBonds(ids:Traversable[String]):Set[Bond] = {
    transaction {
      from(bonds)(bond =>
        where(bond.id in ids)
        select(bond)
      ).toSet
    }
  }
  
  def getBonds(valuedate:JavaDate):Set[Bond] = {
    transaction {
      from(bonds)(bond =>
        where(bond.maturity gt valuedate)
        select(bond)
      ).toSet
    }
  }

  def getBondsByProducts(productids:Traversable[String]):Set[Bond] = 
    transaction {
      from(bonds)(b =>
        where(b.productid in productids)
        select(b)
      ).toSet
    }
  
  def getBondsByIssuers(issuerids:Traversable[String]):Set[Bond] = 
    transaction {
      from(bonds)(b =>
        where(b.issuerid in issuerids)
        select(b)
      ).toSet
    }
  
  def getPriceByParamSet(paramset:String):Set[BondPrice] =
    transaction {
      from(bondprices)(b =>
        where(b.paramset === paramset)
        select(b)
      ).toSet
    }
  
  def getPriceByDate(paramdate:JavaDate):Set[BondPrice] =
    transaction {
      from(bondprices)(b =>
        where(b.paramdate === paramdate)
        select(b)
      ).toSet
    } 
  
  def getPricedParamSets:Set[(String, JavaDate)] =
    transaction {
      from(bondprices)(b =>
        select((&(b.paramset), &(b.paramdate)))
      ).distinct.toSet
    }
  
  def latestPrice:String = getPricedParamSets.maxBy(_._2)._1

  
  /**
   * Returns list of valid paramsets satisfying the given constraints.
   *
   * @param fromDate: A starting point of Date. Range includes this date.
   *                   For example, when you specify this to be Jan 1st, 2012, look-up condition includes Jan 1st, 2012.
   *                   To be concise, the operator used for fromDate in where-clause is "greater than equal."
   * @param toDate: A ending point of Date. Range does not include this date.
   *                 For example, when you specify this to be Jan, 2nd, 2012, look-up condition includes something like 2012-01-01 23:59:59.99999999 etc.
  **/
  def getParamSets:Set[(String, JavaDate)] = getRateFXParamSets & getCDSParamSets
  
  def getParamSets(fromDate:JavaDate, toDate:JavaDate):Set[(String, JavaDate)] = 
    getRateFXParamSets(fromDate, toDate) & getCDSParamSets(fromDate, toDate)
  
  def latestParamSet:String = getParamSets.maxBy(_._2)._1
  
  /**
   * Returns paramsets for each database.
   **/
  def getRateFXParamSets:Set[(String, JavaDate)] = 
    transaction {
        from(ratefxparameters)(p => 
          select(&(p.paramset), &(p.paramdate))).distinct.toSet
    }
  
  def getRateFXParamSets(fromDate:JavaDate, toDate:JavaDate):Set[(String, JavaDate)] = 
    transaction {
        from(ratefxparameters)(p =>
          where((p.paramdate gte fromDate) and (p.paramdate lte toDate))
          select(&(p.paramset), &(p.paramdate))).distinct.toSet
    }
  
  def getCDSParamSets:Set[(String, JavaDate)] = 
    transaction {
        from(cdsparameters)(p => 
          select(&(p.paramset), &(p.paramdate))).distinct.toSet
    }

  def getCDSParamSets(fromDate:JavaDate, toDate:JavaDate):Set[(String, JavaDate)] = 
    transaction {
        from(cdsparameters)(p => 
          where((p.paramdate gte fromDate) and (p.paramdate lte toDate))
          select(&(p.paramset), &(p.paramdate))).distinct.toSet
    }
  
  def getFXParamDates(fromDate:JavaDate, toDate:JavaDate):Set[JavaDate] = 
    transaction {
        from(ratefxparameters)(p =>
          where(
              (p.paramdate gte fromDate) and 
              (p.paramdate lte toDate) and
              (p.instrument === "FX"))
          select(&(p.paramdate))).distinct.toSet
    }

  /**
   * Checks whether the paramset is valid.
   **/
  def isParamSet(id:String):Boolean = {
    getParamSets.map(p => p._1).contains(id)
  }


  /**
   * Returns a List of RateFXParameters that falls onto a range of Dates.
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
   * @return A List of matching RateFXParameters.
   */
  def getRateFXParameters(fromDate:JavaDate, toDate:JavaDate, instrument:String, asset:String, maturity:String):Set[RateFXParameter] = 
    transaction {
      from(ratefxparameters)(ip =>
        where(
          (ip.paramdate  gte fromDate) and
          (ip.paramdate  lt  toDate) and
          ip.instrument === instrument and
          ip.asset      === asset and
          ip.maturity   === maturity
        )
        select(ip)
      ).toSet
    }
  
  def getRateFXParameters(paramset:String):Set[RateFXParameter] =
  	transaction { 
  	  from(ratefxparameters)(c => 
  	    where(c.paramset === paramset)
  	    select(c)
  	    ).toSet
  	}
  
  
  def getRateFXParameters(on:JavaDate, instrument:String, asset:String, maturity:String):Set[RateFXParameter] = 
    transaction {
      from(ratefxparameters)(ip =>
        where(
          ip.paramdate  === on and
          ip.instrument === instrument and
          ip.asset      === asset and
          ip.maturity   === maturity
        )
        select(ip)
      ).toSet
    }
  
  def getRateFXParameters(on:JQuantDate, instrument:String, asset:String, maturity:String):Set[RateFXParameter] = 
    getRateFXParameters(on.longDate, instrument, asset, maturity)

    

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
  def getCDSParameters(fromDate:JavaDate, toDate:JavaDate, maturity:String, issuerid:String, currencyid:String, instrument:String):Set[CDSParameter] = 
    transaction {
      from(cdsparameters)(cds =>
        where(
          (cds.paramdate  gte fromDate) and
          (cds.paramdate  lt  toDate) and
          cds.instrument === instrument and
          cds.maturity   === maturity and
          cds.issuerid   === issuerid and
          cds.currencyid === currencyid
        )
        select(cds)
      ).toSet
    }
  
  def getCDSParameters(fromDate:JavaDate, toDate:JavaDate, maturity:String, issuerid:String, currencyid:String):Set[CDSParameter] = 
    transaction {
      from(cdsparameters)(cds =>
        where(
          (cds.paramdate  gte fromDate) and
          (cds.paramdate  lt  toDate) and
          cds.maturity   === maturity and
          cds.issuerid   === issuerid and
          cds.currencyid === currencyid
        )
        select(cds)
      ).toSet
    }

  def getCDSParameters(on:JavaDate, maturity:String, issuerid:String, currencyid:String):Set[CDSParameter] = 
    transaction {
      from(cdsparameters)(cds =>
        where(
          cds.paramdate  === on and
          cds.issuerid   === issuerid and
          cds.currencyid === currencyid
        )
        select(cds)
      ).toSet
    }

  def getCDSParameters(paramset:String):Set[CDSParameter] = 
    transaction {
        from(cdsparameters)(p => 
          where(p.paramset === paramset)
          select(p)
          ).toSet
    }

//  /**
//   * Returns a List of FX parameters that falls onto a range of Dates.
//   *
//   * @param fromDate A starting point of Date. Range includes this date.
//   *                   For example, when you specify this to be Jan 1st, 2012, look-up condition includes Jan 1st, 2012.
//   *                   To be concise, the operator used for fromDate in where-clause is "greater than equal."
//   * @param toDate A ending point of Date. Range does not include this date.
//   *                 For example, when you specify this to be Jan, 2nd, 2012, look-up condition includes something like 2012-01-01 23:59:59.99999999 etc.
//   *                 To be concise, the operator used for toDate in where-clause is "less than."
//   * @param currencyid target currency (quoted in JPY).
//   * @return A List of matching InputParameters.
//   */
//  def getFXRates(fromDate:JavaDate, toDate:JavaDate, currencyid:String):Set[FXRate] = 
//    transaction {
//      from(fxrates)(ip =>
//        where(
//          (ip.paramdate  gte fromDate) and
//          (ip.paramdate  lt  toDate) and
//          ip.currencyid === currencyid
//        )
//        select(ip)
//      ).toSet
//    }
//  
//  def getFXRates(paramset:String):Set[FXRate] =
//  	transaction { 
//  	  from(fxrates)(c => 
//  	    where(c.paramset === paramset)
//  	    select(c)
//  	    ).toSet
//  	}
//  
//  def getFXRates(on:JavaDate, currencyid:String):Set[FXRate] = 
//    transaction {
//      from(fxrates)(ip =>
//        where(
//          ip.paramdate  === on and
//          ip.currencyid === currencyid
//        )
//        select(ip)
//      ).toSet
//    }
//  
//  def getFXRates(on:JQuantDate, currencyid:String):Set[FXRate] = 
//    getFXRates(on.longDate, currencyid)
//    
//  def getFXRates:Set[FXRate] = {
//    transaction {
//      from(fxrates)(fxrate => select(fxrate)).toSet
//    }
//  }
//  
  def getFXlist:Set[String] = {
    transaction {
      from(ratefxparameters)(p => 
        where (p.instrument === "FX")
        select(&(p.asset))).distinct.toSet
    }
  }
    
  
  /**
   * Returns historical values of RateFX parameter.
   * Note only the "official" parameters (ie. paramset ending with "-000") is taken.
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
   * @return time series of the parameter.
   */
  def getTimeSeries(fromDate:JavaDate, toDate:JavaDate, instrument:String, asset:String, maturity:String):SortedMap[JavaDate, Double] = 
    transaction {
      val qresult = from(ratefxparameters)(ip =>
        where(
          (ip.paramdate gte fromDate) and
          (ip.paramdate lte toDate) and
          (ip.paramset like "%-000") and
          ip.instrument === instrument and
          ip.asset      === asset and
          ip.maturity   === maturity
        )
        select((&(ip.paramdate), &(ip.value)))
      ).toSeq
      TreeMap(qresult : _*)
	 }
  
  def getTimeSeries(fromDate:JavaDate, toDate:JavaDate, instrument:String, asset:String):SortedMap[JavaDate, Double] = 
    transaction {
      val qresult = from(ratefxparameters)(ip =>
        where(
          (ip.paramdate gte fromDate) and
          (ip.paramdate lte toDate) and
          (ip.paramset like "%-000") and
          ip.instrument === instrument and
          ip.asset      === asset
        )
        select((&(ip.paramdate), &(ip.value)))
      ).toSeq
      TreeMap(qresult : _*)
	 }
	    
//  private def EmptyFXSeries(fromDate:JavaDate, toDate:JavaDate):SortedMap[JavaDate, Double] = 
//    TreeMap(getFXParamDates(fromDate, toDate).map(d => (d, 1.0)).toSeq : _*)
    
  /**
   * Returns historical values of FX spot rate against JPY.
   * Note only the "official" parameters (ie. paramset ending with "-000") is taken.
   *
   * @param fromDate A starting point of Date. Range includes this date.
   *                   For example, when you specify this to be Jan 1st, 2012, look-up condition includes Jan 1st, 2012.
   *                   To be concise, the operator used for fromDate in where-clause is "greater than equal."
   * @param toDate A ending point of Date. Range does not include this date.
   *                 For example, when you specify this to be Jan, 2nd, 2012, look-up condition includes something like 2012-01-01 23:59:59.99999999 etc.
   *                 To be concise, the operator used for toDate in where-clause is "less than."
   * @return fx1 Currency to be measured in JPY.
   */
  def getFXTimeSeries(fromDate:JavaDate, toDate:JavaDate, currency:String):SortedMap[JavaDate, Double] 
    = getTimeSeries(fromDate, toDate, "FX", currency)
  
  
  /**
   * Returns historical values of FX spot rate between 2 currencies.
   * Note only the "official" parameters (ie. paramset ending with "-000") is taken.
   *
   * @param fromDate A starting point of Date. Range includes this date.
   *                   For example, when you specify this to be Jan 1st, 2012, look-up condition includes Jan 1st, 2012.
   *                   To be concise, the operator used for fromDate in where-clause is "greater than equal."
   * @param toDate A ending point of Date. Range does not include this date.
   *                 For example, when you specify this to be Jan, 2nd, 2012, look-up condition includes something like 2012-01-01 23:59:59.99999999 etc.
   *                 To be concise, the operator used for toDate in where-clause is "less than."
   * @return fx1 Currency to be measured.
   * @return fx2 Measuring currency.
   * 			 For example, fx1 = "USD" & fx2 = "JPY" returns USD/JPY spot rate = around 80.00
   */
  def getFXTimeSeries(fromDate:JavaDate, toDate:JavaDate, fx1:String, fx2:String):SortedMap[JavaDate, Double] = {
      val fxset1 = getFXTimeSeries(fromDate, toDate, fx1)
      val fxset2 = getFXTimeSeries(fromDate, toDate, fx2)
      TreeMap((fxset1.keySet & fxset2.keySet).map(d => (d, fxset2(d) / fxset1(d))).toSeq : _*)
    }

  /**
   * Returns historical values of CDS.
   * Note only the "official" parameters (ie. paramset ending with "-000") is taken.
   *
   * @param fromDate A starting point of Date. Range includes this date.
   *                   For example, when you specify this to be Jan 1st, 2012, look-up condition includes Jan 1st, 2012.
   *                   To be concise, the operator used for fromDate in where-clause is "greater than equal."
   * @param toDate A ending point of Date. Range does not include this date.
   *                 For example, when you specify this to be Jan, 2nd, 2012, look-up condition includes something like 2012-01-01 23:59:59.99999999 etc.
   *                 To be concise, the operator used for toDate in where-clause is "less than."
   * @return currencyid CDS quotation currency. eg. "USD"
   * @return issuerid Issuer id. eg. "ADB"
   * @return maturity CDS maturity. eg. "6M" or "5Y"
   */
  def getCDSTimeSeries(fromDate:JavaDate, toDate:JavaDate, currencyid:String, issuerid:String, maturity:String):SortedMap[JavaDate, Double] = 
    transaction {
      val qresult = from(cdsparameters)(ip =>
        where(
          (ip.paramdate gte fromDate) and
          (ip.paramdate lte toDate) and
          (ip.paramset like "%-000") and
          ip.issuerid      === issuerid and
          ip.currencyid      === currencyid and
          ip.maturity   === maturity
        )
        select((&(ip.paramdate), &(ip.spread)))
      ).toSeq
      TreeMap(qresult : _*)
    }
  
  /**
   * Returns historical price of a bond, quoted in issue currency.
   * Note only the "official" parameters (ie. paramset ending with "-000") is taken.
   *
   * @param fromDate A starting point of Date. Range includes this date.
   *                   For example, when you specify this to be Jan 1st, 2012, look-up condition includes Jan 1st, 2012.
   *                   To be concise, the operator used for fromDate in where-clause is "greater than equal."
   * @param toDate A ending point of Date. Range does not include this date.
   *                 For example, when you specify this to be Jan, 2nd, 2012, look-up condition includes something like 2012-01-01 23:59:59.99999999 etc.
   *                 To be concise, the operator used for toDate in where-clause is "less than."
   * @return bondid Bond id. eg. "ADB-00001"
   */
  def getPriceTimeSeries(start:JavaDate, end:JavaDate, bondid:String):SortedMap[JavaDate, Double] = 
    transaction {
      val qresult = from(bondprices)(bp =>
        where(
          (bp.paramdate gte start) and
          (bp.paramdate lte end) and
          (bp.paramset like "%-000") and
          bp.instrument === "BONDPRICE" and
          bp.bondid      === bondid and
          bp.priceclean.isNotNull
        )
        select(&(bp.paramdate), &(bp.priceclean.get))
      ).toSeq
      TreeMap(qresult : _*)
  	}
  
  /**
   * Returns historical price of a bond, quoted as JPY percentage value from original fx fixing.
   * Note only the "official" parameters (ie. paramset ending with "-000") is taken.
   * It only returns prices for dates which JPY price is defined.
   *
   * @param fromDate A starting point of Date. Range includes this date.
   *                   For example, when you specify this to be Jan 1st, 2012, look-up condition includes Jan 1st, 2012.
   *                   To be concise, the operator used for fromDate in where-clause is "greater than equal."
   * @param toDate A ending point of Date. Range does not include this date.
   *                 For example, when you specify this to be Jan, 2nd, 2012, look-up condition includes something like 2012-01-01 23:59:59.99999999 etc.
   *                 To be concise, the operator used for toDate in where-clause is "less than."
   * @param bondid target bond id
   * @param defaultfx default fx value in case JPY price is not available for all currencies
   * @return bondid Bond id. eg. "ADB-00001"
   */
  def getJPYPriceTimeSeries(start:JavaDate, end:JavaDate, bondid:String):SortedMap[JavaDate, Double] = 
    transaction {
      val qresult = from(bondprices)(bp =>
        where(
          (bp.paramdate gte start) and
          (bp.paramdate lte end) and
          (bp.paramset like "%-000") and
          bp.instrument === "BONDPRICE" and
          bp.bondid      === bondid and
          bp.priceclean_jpy.isNotNull
        )
        select(&(bp.paramdate), &(bp.priceclean_jpy.get))
      ).toSeq
      
      TreeMap(qresult : _*)
  	}
  
  
  /**
   * Returns historical price of a bond, quoted as JPY percentage value from given fx fixing.
   * Note only the "official" parameters (ie. paramset ending with "-000") is taken.
   * It returns prices for dates which price in original currency is defined.
   *
   * @param fromDate A starting point of Date. Range includes this date.
   *                   For example, when you specify this to be Jan 1st, 2012, look-up condition includes Jan 1st, 2012.
   *                   To be concise, the operator used for fromDate in where-clause is "greater than equal."
   * @param toDate A ending point of Date. Range does not include this date.
   *                 For example, when you specify this to be Jan, 2nd, 2012, look-up condition includes something like 2012-01-01 23:59:59.99999999 etc.
   *                 To be concise, the operator used for toDate in where-clause is "less than."
   * @param bondid target bond id
   * @param defaultfx default fx value in case JPY price is not available for all currencies
   * @param fx	
   * @return bondid Bond id. eg. "ADB-00001"
   */
  def getJPYPriceTimeSeries(start:JavaDate, end:JavaDate, bondid:String, basefx:Double):SortedMap[JavaDate, Double] = 
    transaction {
      val qresult = from(bondprices)(bp =>
        where(
          (bp.paramdate gte start) and
          (bp.paramdate lte end) and
          (bp.paramset like "%-000") and
          bp.instrument === "BONDPRICE" and
          bp.bondid      === bondid and
          bp.priceclean.isNotNull
        )
        select(&(bp.paramdate), &(bp.priceclean.get))
      ).toMap
      
      val quoteccy = from(bonds)(b => 
        where (b.id === bondid)
        select (&(b.currencyid))
        ).singleOption
      
      if (quoteccy.isEmpty) TreeMap.empty[JavaDate, Double]
      else
      {
	      val fxseries = getFXTimeSeries(start, end, quoteccy.get, "JPY")
	      val commondates = fxseries.keySet & qresult.keySet
	      val result = commondates.map{ d => (d, qresult(d) * fxseries(d) / basefx) }
	      TreeMap(result.toSeq : _*)
      }
  	}
  
  
  /**
   * Inserts bond prices to the database.
   *
   * @param objects A List of Squeryl model objects.
   * @return Whether or not the statement ran successfully.
   *          However, this does not guarantee whether every row has been inserted.
   */
  def insertOrUpdate[T <: KeyedEntity[String]](data:Traversable[T], overwrite:Boolean):Boolean = {
    
    if (data.isEmpty) return false
    
    val datatable = data.head.getClass.getSimpleName.toString match {
      case "BondPrice" => bondprices
      case "Volatility" => volatilities
      case "Correlation" => correlations
      case "Coupon" => coupons
      case "ForwardPrice" => forwardprices
      case _ => null
    }
    
    if (datatable == null) return false
    insertMany(data.toSet, overwrite)
  }

  /**
   * Inserts bond prices to the database.
   *
   * @param objects A List of Squeryl model objects.
   * @return Whether or not the statement ran successfully.
   *          However, this does not guarantee whether every row has been inserted.
   */
  def empty[T <: KeyedEntity[String]](table:Table[T]):Boolean = {
    val tablename = table.name
    runSQLStatement("TRUNCATE TABLE " + tablename)
    true
  }
  
  /**
   * Inserts many Squeryl model objects via CSV.
   *
   * @param objects A List of Squeryl model objects.
   * @return Whether or not the statement ran successfully.
   *          However, this does not guarantee whether every row has been inserted.
   */
  def insertMany(objects:Traversable[AnyRef], overwrite:Boolean):Boolean = {
    val csv = buildCSVImportStatement(objects.toSet, overwrite)
    csv.foreach(runSQLStatement)
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
  def buildCSVImportStatement(objects:Set[AnyRef], overwrite:Boolean):List[String] =  {
    val tempFile            = File.createTempFile("squantlib", ".csv")
    tempFile.deleteOnExit()
    val tempFilePath        = tempFile.getAbsolutePath
    val tableNames          = tables.toList.map(t => t.posoMetaData.clasz.getSimpleName)
    val clazz               = objects.head.getClass
    val className           = clazz.getSimpleName.toString()
    val table               = tables(tableNames.indexOf(className))
    val tableName           = tables(tableNames.indexOf(className)).name
    val attrToField         = table.posoMetaData.fieldsMetaData.toList.map(fmd => (fmd.nameOfProperty, fmd.columnName))
    val builder             = new StringBuilder()
    val columnNames         = attrToField.map(pair => pair._2)
    builder.append(columnNames.mkString(",") + "\n")
    for (obj <- objects) {
      builder.append(attrToField.map(pair => quoteValue(clazz.getMethod(pair._1).invoke(obj))).mkString(","))
      builder.append("\n")
    }
    val out = new BufferedWriter(new FileWriter(tempFile))
    out.write(builder.toString)
    out.close()

    List(
      "START TRANSACTION;",
      "SET FOREIGN_KEY_CHECKS = 1;",
      "LOAD DATA LOCAL INFILE '" + tempFilePath.replaceAll("\\\\", "\\\\\\\\") + "' " +
        (if(overwrite) "REPLACE " else "") + "INTO TABLE " + tableName + " " +
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
  def quoteList(value:Traversable[Any]):String = "(" + value.map(quoteValue).mkString(",") + ")"

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

  /**
   * Parses a risktag map from a string. Risktag is a Map[String, Int] structure where String represents a risk factor,
   * and Int represents the weight of the factor, for instance, Map("euro-econ", 10, "france" -> 5).
   *
   * @param value String in "tag=weight; tag=weight; ..." format.
   * @return A risktag Map
   */
  def loadRisktags(value:String):TreeMap[String, Int] = {
    TreeMap(value.split(";").map(s => s.trim.split("=").map(t => t.trim)).map(s => (s(0), s(1).toInt)):_*)
  }

  /**
   * Dumps a risktag into a string.
   *
   * @param value A risktag Map
   * @return String in "tag=weight; tag=weight; ..." format.
   */
  def dumpRisktags(value:TreeMap[String, Int]):String = {
    value.map(t => t._1 + "=" + t._2).mkString("; ")
  }
}
