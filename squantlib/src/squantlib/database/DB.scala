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
//import scala.collection.immutable.TreeMap
//import scala.collection.SortedMap
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
    dataSource.setCheckoutTimeout(10000)
    SessionFactory.concreteFactory = Some(() => {
      Session.create(dataSource.getConnection, new MySQLInnoDBAdapter)
    })
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
  val ratefxparameters = table[RateFXParameter]("RateFXParameters")
  val cdsparameters = table[CDSParameter]("CDSParameters")
  val bondprices = table[BondPrice]("BondPrices")
  val volatilities = table[Volatility]("Volatilities")
  val correlations = table[Correlation]("Correlations")
  val coupons = table[Coupon]("Coupons")
  val forwardprices = table[ForwardPrice]("ForwardPrices")
  val impliedrates = table[ImpliedRate]("ImpliedRates")

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
  
  def getPricedParamSets(fromDate:JavaDate, toDate:JavaDate):Set[(String, JavaDate)] =
    transaction {
      from(bondprices)(b =>
        where (b.paramdate between (fromDate, toDate))
        select((&(b.paramset), &(b.paramdate)))
      ).distinct.toSet
    }
  
  def getPricedBonds:Set[String] =
    transaction {
      from(bondprices)(b =>
        select(&(b.bondid))
      ).distinct.toSet
    }
  
  def getVolatilityDates:Set[JavaDate] =
    transaction {
      from(volatilities)(b =>
        select(&(b.valuedate))
      ).distinct.toSet
    }
  
  def getCorrelationDates:Set[JavaDate] =
    transaction {
      from(correlations)(b =>
        select(&(b.valuedate))
      ).distinct.toSet
    }
  
  def getForwardPriceParams:Set[String] =
    transaction {
      from(forwardprices)(p =>
        select(&(p.paramset))
      ).distinct.toSet
    }
  
  def getImpliedRateParams:Set[String] =
    transaction {
      from(impliedrates)(p =>
        select(&(p.paramset))
      ).distinct.toSet
    }
  
  def getVolUnderlyings:Set[String] =
    transaction {
      from(volatilities)(b =>
        select(&(b.underlying))
      ).distinct.toSet
    }
  
  def getVolatilities(fromDate:JavaDate, toDate:JavaDate):Set[Volatility] = {
    transaction {
      from(volatilities)(vol =>
        where(vol.valuedate between (fromDate, toDate))
        select(vol)
      ).toSet
    }
  }
  
  def getCoupons(bondid:String):List[Coupon] = getCoupons(Set(bondid))
  
  def getCoupons(bondids:Traversable[String]):List[Coupon] = 
    transaction {
      from(coupons)(c =>
        where(c.bondid in bondids)
        select(c)
      ).toList
    }
  
  def getUnfixedCoupons(d:JavaDate):List[Coupon] = 
    transaction {
      from(coupons)(c =>
        where(
          (c.eventdate lte d) and
          (c.rate isNotNull) and
          not (c.rate === "") and
          ((c.fixedrate.isNull) or (c.fixedamount.isNull))
        )
        select(c)
      ).toList
    }
  
  def getDefinedCoupons(d:JavaDate):List[Coupon] = 
    transaction {
      from(coupons)(c =>
        where(
          (c.eventdate lte d) and
          (c.rate isNotNull) and
          not (c.rate === "")
        )
        select(c)
      ).toList
    }
  
  def getPreviousCoupons(bondid:String, d:JavaDate):List[Coupon] = 
    transaction {
      from(coupons)(c =>
        where(
            (c.bondid === bondid) and
            (c.paymentdate lte d) and
            (c.paymenttype === "COUPON"))
        select(c)
      ).toList
    }
  
  def getPreviousCoupons(bondid:String, d:JavaDate, nb:Int):List[Coupon] = {
    val cpns = getPreviousCoupons(bondid, d)
    if (cpns.size <= nb) cpns
    else cpns.toList.sortBy(_.paymentdate).takeRight(nb).toList
  }
  
  def getCurrentCoupons(bondid:String, d:JavaDate):List[Coupon] = 
    transaction {
      from(coupons)(c =>
        where(
            (c.bondid === bondid) and
            (c.startdate lt d) and 
            (c.enddate gte d) and
            (c.paymenttype === "COUPON"))
        select(c)
      ).toList
    }
  
  def getCurrentAndPreviousCoupons(bondid:String, d:JavaDate):(List[Coupon], List[Coupon]) = 
    transaction {
      val current = from(coupons)(c =>
        where(
            (c.bondid === bondid) and
            (c.startdate lt d) and 
            (c.enddate gte d) and
            (c.paymenttype === "COUPON"))
        select(c)
      ).toList
      
      val previous = from(coupons)(c =>
        where(
            (c.bondid === bondid) and
            (c.paymentdate lte d) and
            (c.paymenttype === "COUPON"))
        select(c)
      ).toList
      
      (current, previous)
    }
  
  def getCurrentAndPreviousCoupons(bondid:String, d:JavaDate, nb:Int):(List[Coupon], List[Coupon]) = {
    val (current, previous) = getCurrentAndPreviousCoupons(bondid, d)
    if (previous.size <= nb) (current, previous)
    else (current, previous.toList.sortBy(_.paymentdate).takeRight(nb))
  }
    
//  def getCoupons(d:JavaDate):Set[Coupon] = 
//    transaction {
//      from(coupons)(c =>
//        where(c.eventdate lte d)
//        select(c)
//      ).toSet
//    }
  
  def getCouponMissingBonds:Set[Bond] = 
    transaction {
      val couponbonds = from(coupons)(c => select(&(c.bondid))).distinct.toSet
      if (couponbonds.isEmpty) getBonds
      else
	  from (bonds)(b => 
	    where (not(b.id in couponbonds))
	    select(b)).distinct.toSet
    }
  
  def getLatestPrices:Set[BondPrice] = getPriceByParamSet(getLatestPriceParam._1)
  def getLatestPriceParam:(String, JavaDate) = getPricedParamSets.maxBy(_._2)
  def getLatestVolatilityDate:JavaDate = if (getVolatilityDates.isEmpty) null else getVolatilityDates.max
  def getLatestCorrelationDate:JavaDate = if (getCorrelationDates.isEmpty) null else getCorrelationDates.max
  
  def getVolatilityBondUnderlyings = getVolUnderlyings.withFilter(_.substring(0, 6) == "PRICE:").map(_.substring(6))
  def getVolatilityFXUnderlyings = getVolUnderlyings.withFilter(_.substring(0, 3) == "FX:").map(_.substring(3))
  
  /**
   * Returns list of valid paramsets satisfying the given constraints.
   *
   * @param fromDate: A starting point of Date. Range includes this date.
   *                   For example, when you specify this to be Jan 1st, 2012, look-up condition includes Jan 1st, 2012.
   *                   To be concise, the operator used for fromDate in where-clause is "greater than equal."
   * @param toDate: A ending point of Date. Range does not include this date.
   *                 For example, when you specify this to be Jan, 2nd, 2012, look-up condition includes something like 2012-01-01 23:59:59.99999999 etc.
  **/
  def getParamSets:Set[(String, JavaDate)] = getRateFXParamSets & getCDSParamSets & getFXParamSets
  
  def getParamSets(fromDate:JavaDate, toDate:JavaDate):Set[(String, JavaDate)] = 
    getRateFXParamSets(fromDate, toDate) & getCDSParamSets(fromDate, toDate) & getFXParamSets(fromDate, toDate)
    
  def getParamSetsAfter(basedate:JavaDate):Set[(String, JavaDate)] = 
    getParamSets.filter{case (pset, pdate) => pdate after basedate}
  
  def getLatestParamSet:(String, JavaDate) = getParamSets.maxBy(_._2)
  
  /**
   * Returns paramsets for each database.
   **/
  def getFXParamSets:Set[(String, JavaDate)] = 
    transaction {
        from(fxrates)(p => 
          select(&(p.paramset), &(p.paramdate))).distinct.toSet
    }
  
  def getFXParamSets(fromDate:JavaDate, toDate:JavaDate):Set[(String, JavaDate)] = 
    transaction {
        from(fxrates)(p =>
          where(p.paramdate between (fromDate, toDate))
          select(&(p.paramset), &(p.paramdate))).distinct.toSet
    }
  
  def getRateFXParamSets:Set[(String, JavaDate)] = 
    transaction {
        from(ratefxparameters)(p => 
          select(&(p.paramset), &(p.paramdate))).distinct.toSet
    }
  
  def getRateFXParamSets(fromDate:JavaDate, toDate:JavaDate):Set[(String, JavaDate)] = 
    transaction {
        from(ratefxparameters)(p =>
          where(p.paramdate between (fromDate, toDate))
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
          where(p.paramdate between (fromDate, toDate))
          select(&(p.paramset), &(p.paramdate))).distinct.toSet
    }
  
  def getFXParamDates(fromDate:JavaDate, toDate:JavaDate):Set[JavaDate] = 
    transaction {
        from(ratefxparameters)(p =>
          where(
              (p.paramdate between (fromDate, toDate)) and
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

    
  def getRateFX(paramset:String, instrument:String, asset:String, maturity:String):Option[Double] = 
    transaction {
      from(ratefxparameters)(ip =>
        where(
          ip.paramset === paramset  and
          ip.instrument === instrument and
          ip.asset      === asset and
          ip.maturity   === maturity
        )
        select(&(ip.value))
      ).firstOption
    }

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
  def getTimeSeries(fromDate:JavaDate, toDate:JavaDate, instrument:String, asset:String, maturity:String):Map[JavaDate, Double] = 
    transaction {
      from(ratefxparameters)(ip =>
        where(
          (ip.paramdate gte fromDate) and
          (ip.paramdate lte toDate) and
          (ip.paramset like "%-000") and
          ip.instrument === instrument and
          ip.asset      === asset and
          ip.maturity   === maturity
        )
        select((&(ip.paramdate), &(ip.value)))
      ).toMap
	 }
  
  def getTimeSeries(fromDate:JavaDate, toDate:JavaDate, instrument:String, asset:String):Map[JavaDate, Double] = 
    transaction {
      from(ratefxparameters)(ip =>
        where(
          (ip.paramdate gte fromDate) and
          (ip.paramdate lte toDate) and
          (ip.paramset like "%-000") and
          ip.instrument === instrument and
          ip.asset      === asset
        )
        select((&(ip.paramdate), &(ip.value)))
      ).toMap
	 }

  
  def getTimeSeries(instrument:String, asset:String):Map[JavaDate, Double] = 
    transaction {
      from(ratefxparameters)(ip =>
        where(
          (ip.paramset like "%-000") and
          ip.instrument === instrument and
          ip.asset      === asset
        )
        select((&(ip.paramdate), &(ip.value)))
      ).toMap
	 }
  
  def getTimeSeries(instrument:String, asset:String, maturity:String):Map[JavaDate, Double] = 
    transaction {
      from(ratefxparameters)(ip =>
        where(
          (ip.paramset like "%-000") and
          ip.instrument === instrument and
          ip.asset      === asset and
          ip.maturity   === maturity
        )
        select((&(ip.paramdate), &(ip.value)))
      ).toMap
	 }
  
  def getLatestParam(instrument:String, asset:String, maturity:String, valuedate:JavaDate):Option[(JavaDate, Double)] = 
    transaction {
      from(ratefxparameters)(ip =>
        where(
          (ip.paramset like "%-000") and
          ip.instrument === instrument and
          ip.asset      === asset and
          ip.maturity   === maturity and
          (ip.paramdate lte valuedate)
        )
        select((&(ip.paramdate), &(ip.value)))
        orderBy (ip.paramdate desc)).page(0, 1).headOption
	 }
  
  def getLatestParam(instrument:String, asset:String, valuedate:JavaDate):Option[(JavaDate, Double)] = 
    transaction {
      from(ratefxparameters)(ip =>
        where(
          (ip.paramset like "%-000") and
          ip.instrument === instrument and
          ip.asset      === asset and
          (ip.paramdate lte valuedate)
        )
        select((&(ip.paramdate), &(ip.value)))
        orderBy (ip.paramdate desc)).page(0, 1).headOption
	 }
  
  
  
  def getFX(paramset:String, ccy:String):Option[Double] = 
    transaction {
      from(fxrates)(ip =>
        where(
          ip.paramset === paramset  and
          ip.currencyid === ccy
        )
        select(&(ip.fxjpy))
      ).firstOption
    }
  
  def getFX(paramset:String, ccy:String, ccy2:String):Option[Double] = {
    val fx1 = getFX(paramset, ccy)
    val fx2 = getFX(paramset, ccy2)
    if (fx1.isDefined && fx2.isDefined) Some(fx2.get / fx1.get) else None
  }
  
  
  /**
   * Returns list of all currencies in the FX database (against JPY).
   */
  def getFXlist:Set[String] = 
    transaction {
        from(fxrates)(fx => select(&(fx.currencyid))).distinct.toSet
    }
  
  /**
   * Returns historical values of FX spot rate against JPY.
   * Note only the "official" parameters (ie. paramset ending with "-000") is taken.
   *
   * @return fx1 Currency to be measured in JPY.
   */
  def getFXTimeSeries(ccy:String):Map[JavaDate, Double] = 
    transaction {
      from(fxrates)(fx =>
        where(
          (fx.paramset like "%-000") and
          (fx.currencyid === ccy)
        )
        select((&(fx.paramdate), &(fx.fxjpy)))
      ).toMap
	 }
  
  /**
   * Returns historical values of FX spot rate.
   * Note only the "official" parameters (ie. paramset ending with "-000") is taken.
   *
   * @return fx1 Currency to be measured in JPY.
   */
  def getFXTimeSeries(currency1:String, currency2:String):Map[JavaDate, Double] = {
	  if (currency2 == "JPY") getFXTimeSeries(currency1)
	  else {
	      val fxset1 = getFXTimeSeries(currency1)
	      val fxset2 = getFXTimeSeries(currency2)
	      (fxset1.keySet & fxset2.keySet).map(d => (d, fxset2(d) / fxset1(d))).toMap
	  }
    }
  
  /**
   * Returns historical values of FX spot rate.
   * Note only the "official" parameters (ie. paramset ending with "-000") is taken.
   *
   * @return fx1 Currency to be measured in JPY.
   */
  def getFXTimeSeries(currencylist:Set[String]):Map[String, Map[JavaDate, Double]] = {
	    transaction {
	      currencylist.map( c =>
	        (c, 
		      from(fxrates)(p =>
		        where(
		            (p.paramset like "%-000") and 
		            (p.currencyid === c))
		        select((&(p.paramdate), &(p.fxjpy)))
		      ).toMap
		      )).toMap
		 }
    }
  
  
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
  def getFXTimeSeries(fromDate:JavaDate, toDate:JavaDate, currency:String):Map[JavaDate, Double] =
    transaction {
      from(fxrates)(fx =>
        where(
          (fx.paramset like "%-000") and
          (fx.currencyid === currency) and
          (fx.paramdate gte fromDate) and
          (fx.paramdate lte toDate)
        )
        select((&(fx.paramdate), &(fx.fxjpy)))
      ).toMap
	 }
    
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
  def getFXTimeSeries(fromDate:JavaDate, toDate:JavaDate, currency1:String, currency2:String):Map[JavaDate, Double] = {
	  if (currency2 == "JPY") getFXTimeSeries(fromDate, toDate, currency1)
	  else {
	      val fxset1 = getFXTimeSeries(fromDate, toDate, currency1)
	      val fxset2 = getFXTimeSeries(fromDate, toDate, currency2)
	      (fxset1.keySet & fxset2.keySet).map(d => (d, fxset2(d) / fxset1(d))).toMap
	  }
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
  def getCDSTimeSeries(fromDate:JavaDate, toDate:JavaDate, currencyid:String, issuerid:String, maturity:String):Map[JavaDate, Double] = 
    transaction {
      from(cdsparameters)(ip =>
        where(
          (ip.paramdate gte fromDate) and
          (ip.paramdate lte toDate) and
          (ip.paramset like "%-000") and
          ip.issuerid      === issuerid and
          ip.currencyid      === currencyid and
          ip.maturity   === maturity
        )
        select((&(ip.paramdate), &(ip.spread)))
      ) toMap
//      SortedMap(qresult : _*)
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
  def getPriceTimeSeries(bondid:String):Map[JavaDate, Double] = 
    transaction {
      from(bondprices)(bp =>
        where(
          (bp.paramset like "%-000") and
          bp.instrument === "BONDPRICE" and
          bp.bondid      === bondid and
          bp.priceclean.isNotNull
        )
        select(&(bp.paramdate), &(bp.priceclean.get))
      ) toMap;
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
  def getPriceTimeSeries(start:JavaDate, end:JavaDate, bondid:String):Map[JavaDate, Double] = 
    transaction {
      from(bondprices)(bp =>
        where(
          (bp.paramdate gte start) and
          (bp.paramdate lte end) and
          (bp.paramset like "%-000") and
          bp.instrument === "BONDPRICE" and
          bp.bondid      === bondid and
          bp.priceclean.isNotNull
        )
        select(&(bp.paramdate), &(bp.priceclean.get))
      ) toMap;
//      SortedMap(qresult : _*)
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
  def getJPYPriceTimeSeries(start:JavaDate, end:JavaDate, bondid:String):Map[JavaDate, Double] = 
    transaction {
      from(bondprices)(bp =>
        where(
          (bp.paramdate gte start) and
          (bp.paramdate lte end) and
          (bp.paramset like "%-000") and
          bp.instrument === "BONDPRICE" and
          bp.bondid      === bondid and
          bp.priceclean_jpy.isNotNull
        )
        select(&(bp.paramdate), &(bp.priceclean_jpy.get))
      ).toMap
  	}
  
  /**
   * Returns historical price of a bond, quoted as JPY percentage value from given fx fixing.
   * Note only the "official" parameters (ie. paramset ending with "-000") is taken.
   * It returns prices for dates which price in original currency is defined.
   *
   * @param bondid target bond id
   * @param defaultfx default fx value in case JPY price is not available for all currencies
   * @param fx	
   * @return bondid Bond id. eg. "ADB-00001"
   */
  def getJPYPriceTimeSeries(bondid:String, basefx:Double):Map[JavaDate, Double] = 
    transaction {
      val qresult = from(bondprices)(bp =>
        where(
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
      
      if (quoteccy.isEmpty) Map.empty[JavaDate, Double]
      else
      {
	      val fxseries = getFXTimeSeries(quoteccy.get)
	      val commondates = fxseries.keySet & qresult.keySet
	      commondates.map{ d => (d, qresult(d) * fxseries(d) / basefx) }.toMap
      }
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
  def getJPYPriceTimeSeries(start:JavaDate, end:JavaDate, bondid:String, basefx:Double):Map[JavaDate, Double] = 
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
      
      if (quoteccy.isEmpty) Map.empty[JavaDate, Double]
      else
      {
	      val fxseries = getFXTimeSeries(start, end, quoteccy.get, "JPY")
	      val commondates = fxseries.keySet & qresult.keySet
	      commondates.map{ d => (d, qresult(d) * fxseries(d) / basefx) }.toMap
	      
//	      TreeMap(result.toSeq : _*)
      }
  	}
  
  
  /**
   * Inserts bond prices to the database.
   *
   * @param objects A List of Squeryl model objects.
   * @return Whether or not the statement ran successfully.
   *          However, this does not guarantee whether every row has been inserted.
   */
  def insertOrUpdate[T <: KeyedEntity[String]](data:Traversable[T], overwrite:Boolean):Int = {
    
    if (data.isEmpty) return 0
    
    val datatable = data.head.getClass.getSimpleName.toString match {
      case "BondPrice" => bondprices
      case "Volatility" => volatilities
      case "Correlation" => correlations
      case "Coupon" => coupons
      case "ForwardPrice" => forwardprices
      case "ImpliedRate" => impliedrates
      case _ => null
    }
    
    if (datatable == null) return 0
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
  def insertMany(objects:Set[AnyRef], overwrite:Boolean):Int = {
    val csv = buildCSVImportStatement(objects, overwrite)
    var result = 0
    csv.foreach(x => result += runSQLUpdateStatement(x))
    result
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
    val result = preparedStatement.execute
    if (dataSource.getNumBusyConnections > 1) session.close
    result
  }
  
  /**
   * Runs a SQL update statement.
   *
   * @param statement A SQL statement to run.
   * @return Number of impacted rows
   */
  def runSQLUpdateStatement(statement:String):Int = {
    val session           = SessionFactory.concreteFactory.get()
    val preparedStatement = session.connection.prepareStatement(statement)
    val result = preparedStatement.executeUpdate
    if (dataSource.getNumBusyConnections > 3) session.close
    result
  }
  

  /**
   * Builds MySQL CSV import statement from multiple Squeryl objects.
   * CSV file for the objects will be automatically generated, and embedded in INFILE-clause in the query.
   *
   * @param objects List of a Squeryl objects of a same Model, such as List[BondPrice]
   * @return A List of strings of prepared SQL statement.
   */
  def buildCSVImportStatement(objects:Set[AnyRef], overwrite:Boolean, batchsize:Int = 100000):List[String] =  {
	val tableNames          = tables.toList.map(t => t.posoMetaData.clasz.getSimpleName)
	val clazz               = objects.head.getClass
    val className           = clazz.getSimpleName.toString()
    val table               = tables(tableNames.indexOf(className))
    val tableName           = tables(tableNames.indexOf(className)).name
    val attrToField         = table.posoMetaData.fieldsMetaData.toList.map(fmd => (fmd.nameOfProperty, fmd.columnName))
	val columnNames         = attrToField.map(pair => pair._2)
	
    val tempfiles:List[String] = objects.grouped(batchsize).map(objs => {
	    val tempFile            = File.createTempFile("squantlib", ".csv")
	    tempFile.deleteOnExit()
	    val tempFilePath        = tempFile.getAbsolutePath
	    val builder             = new StringBuilder()
	    builder.append(columnNames.mkString(",") + "\n")
	    for (obj <- objs) {
	      builder.append(attrToField.map(pair => quoteValue(clazz.getMethod(pair._1).invoke(obj))).mkString(","))
	      builder.append("\n")
	    }
	    val out = new BufferedWriter(new FileWriter(tempFile))
	    out.write(builder.toString)
	    out.close()
	    tempFilePath
    }).toList
    
    val insertstatement = tempfiles.map(f => 
      "LOAD DATA LOCAL INFILE '" + f.replaceAll("\\\\", "\\\\\\\\") + "' " +
        (if(overwrite) "REPLACE " else "") + "INTO TABLE " + tableName + " " +
        "FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '''' " +
        "IGNORE 1 LINES " +
        "(" + columnNames.mkString(", ") + ")" + ";"
    )
    
    List(
      "START TRANSACTION;", "SET FOREIGN_KEY_CHECKS = 1;") ++ insertstatement ++ List("COMMIT;")
//      "LOAD DATA LOCAL INFILE '" + tempFilePath.replaceAll("\\\\", "\\\\\\\\") + "' " +
//        (if(overwrite) "REPLACE " else "") + "INTO TABLE " + tableName + " " +
//        "FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '''' " +
//        "IGNORE 1 LINES " +
//        "(" + columnNames.mkString(", ") + ")" + ";",
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
  def loadRisktags(value:String):Map[String, Int] = {
    value.split(";").map(s => s.trim.split("=").map(t => t.trim)).map(s => (s(0), s(1).toInt)).toMap
  }

  /**
   * Dumps a risktag into a string.
   *
   * @param value A risktag Map
   * @return String in "tag=weight; tag=weight; ..." format.
   */
  def dumpRisktags(value:Map[String, Int]):String = {
    value.map(t => t._1 + "=" + t._2).mkString("; ")
  }
}
