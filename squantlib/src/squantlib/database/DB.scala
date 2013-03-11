package squantlib.database

import java.sql.{Timestamp, Time, DriverManager}
import com.mysql.jdbc.Driver
import com.mchange.v2.c3p0._
import org.squeryl.dsl.{TypedExpression, TDate, TInt, TypedExpressionFactory}
import org.squeryl.dsl.ast.FunctionNode
import org.squeryl.adapters._
import org.squeryl.{Session, SessionFactory, Schema, Table}
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.{KeyedEntity, Table}
import squantlib.database.schemadefinitions._
import scala.collection.mutable.{MutableList, StringBuilder}
import org.jquantlib.time.{Date => JQuantDate}
import java.io.{File, FileWriter, BufferedWriter, FileInputStream}
import java.util.{Date => JavaDate, Calendar => JavaCalendar, UUID, Properties}
import java.text.SimpleDateFormat
import org.apache.commons.lang3.StringEscapeUtils

object DB extends Schema { 

  private val properties = new Properties
  val dataSource = new ComboPooledDataSource

  /**
   * Sets up the DB connection for current thread from properties file
   * 
   * @param uri A connection string such as mysql://your.mysql.server:3128/database_name
   * @param username A username to MySQL
   * @param password Password for the user above
   *
   */
  def setup(propertiesPath:String):Unit = {
    properties.load(new FileInputStream(propertiesPath))
    setup(properties.get("uri").toString, properties.get("username").toString, properties.get("password").toString)
  }
  
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
    dataSource.setMaxIdleTime(60 * 30)
    dataSource.setIdleConnectionTestPeriod(30)
    SessionFactory.concreteFactory = Some(() => {
      Session.create(dataSource.getConnection, new MySQLInnoDBAdapter {override def quoteIdentifier(s:String):String = "`" + s + "`"})
    })
  }

  def reconnect:Unit = {
    setup(properties.get("uri").toString, properties.get("username").toString, properties.get("password").toString)
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
  val inputparameters = table[InputParameter]("InputParameters")
  val cdsparameters = table[CDSParameter]("CDSParameters")
  val bondprices = table[BondPrice]("BondPrices")
  val volatilities = table[Volatility]("Volatilities")
  val correlations = table[Correlation]("Correlations")
  val coupons = table[Coupon]("Coupons")
  val forwardprices = table[ForwardPrice]("ForwardPrices")
  val impliedrates = table[ImpliedRate]("ImpliedRates")
  val underlyings = table[Underlying]("Underlyings")
  
  
  private def getKeyedEntity[A<:KeyedEntity[String]](t:Table[A]):Set[A] = transaction {
      from(t)(p => select(p)).toSet}
  
  private def getKeyedEntity[A<:KeyedEntity[String]](t:Table[A], ids:Traversable[String]):Set[A] = transaction {
      from(t)(p => where(p.id in ids) select(p)).toSet }
  
  private def getAKeyedEntity[A<:KeyedEntity[String]](t:Table[A], id:String):Option[A] = transaction {
      from(t)(p => where(p.id === id) select(p)).headOption }
  
//  private def weekday(b: TypedExpression[java.util.Date,TDate])
//  (implicit f: TypedExpressionFactory[Int,TInt]) = f.convert(new FunctionNode("WEEKDAY", Seq(b)))
  
  private implicit def dateToComparableDate(d:JavaDate):comparableDate = new comparableDate(d)
  class comparableDate(val date:JavaDate) extends JavaDate {
    def >(d:JavaDate):Boolean = date.after(d)
    def <(d:JavaDate):Boolean = date.before(d)
    def >=(d:JavaDate):Boolean = !date.before(d)
    def <=(d:JavaDate):Boolean = !date.after(d)
  }
  

  /**
   * Returns a Set of Country objects identified by a List of ID.
   * 
   * @param ids A List of unique IDs.
   * @return A Set of Country objects.
   */
  def getCountries:Set[Country] = getKeyedEntity(countries)
  def getCountries(ids:Traversable[String]):Set[Country] = getKeyedEntity(countries, ids)

  /**
   * Returns a Set of Currencies objects identified by a Set of ID.
   * 
   * @param ids A Set of unique IDs.
   * @return A Set of Currency objects.
   */
  def getCurrencies:Set[Currency] = getKeyedEntity(currencies)
  def getCurrencies(ids:Traversable[String]):Set[Currency] = getKeyedEntity(currencies, ids)
  
  def getCurrencyShortJNames:Map[String, String] = transaction {
      from(currencies)(c => select((&(c.id), &(c.name_jpn_short)))).toMap}

  def getCurrencyJNames:Map[String, String] = transaction {
      from(currencies)(c => select((&(c.id), &(c.name_jpn)))).toMap}

  /**
   * Returns a Set of Distributors objects identified by a Set of ID.
   * 
   * @param ids A Set of unique IDs.
   * @return A Set of Distributor objects.
   */
  def getDistributors:Set[Distributor] = getKeyedEntity(distributors)
  def getDistributors(ids:Traversable[String]):Set[Distributor] = getKeyedEntity(distributors, ids)

  /**
   * Returns a Set of Issuer objects identified by a Set of ID.
   * 
   * @param ids A Set of unique IDs.
   * @return A Set of Issuer objects.
   */
  def getIssuers:Set[Issuer] = getKeyedEntity(issuers)
  def getIssuers(ids:Traversable[String]):Set[Issuer] = getKeyedEntity(issuers, ids)
  
  /**
   * Returns Underlying information
   */ 
  def getUnderlyings:Set[Underlying] = getKeyedEntity(underlyings)
  def getUnderlyings(ids:Traversable[String]):Set[Underlying] = getKeyedEntity(underlyings, ids)
  def getUnderlying(id:String):Option[Underlying] = getAKeyedEntity(underlyings, id)
  
  /**
   * Returns a Set of Product objects identified by a Set of ID.
   * 
   * @param ids A Set of unique IDs.
   * @return A Set of Product objects.
   */
  def getProducts:Set[Product] = getKeyedEntity(products)
  def getProducts(ids:Traversable[String]):Set[Product] = getKeyedEntity(products, ids)
  
  /**
   * Returns a Set of Bond objects identified by a Set of ID.
   * 
   * @param ids A Set of unique IDs.
   * @return A Set of Bond objects.
   */

  def getBond(id:String):Option[Bond] = getBonds(Set(id)) match {
    case b if b.isEmpty => None
    case b => Some(b.head)}
  
  def getBonds:Set[Bond] = getKeyedEntity(bonds)
  def getBonds(ids:Traversable[String]):Set[Bond] = getKeyedEntity(bonds, ids)
  
  def getBonds(valuedate:JavaDate):Set[Bond] = transaction {
      from(bonds)(bond => where(bond.maturity gt valuedate) select(bond)).toSet}

  def getBondsByProducts(productids:Traversable[String]):Set[Bond] = transaction {
      from(bonds)(b => where(b.productid in productids) select(b)).toSet}
  
  def getBondsByIssuers(issuerids:Traversable[String]):Set[Bond] = transaction {
      from(bonds)(b => where(b.issuerid in issuerids) select(b)).toSet}

  def getCouponMissingBonds:Set[Bond] = 
    transaction {
      val couponbonds = from(coupons)(c => select(&(c.bondid))).distinct.toSet
      if (couponbonds.isEmpty) getBonds
      else  from (bonds)(b => 
		    where (not(b.id in couponbonds))
		    select(b)).distinct.toSet
    }
  
  def getBondIds:Set[String] = transaction {from (bonds) (b => select (&(b.id))).toSet}
  
  def setBondPriceTag(tag:Option[Int], id:String):Unit = transaction{
    update(bonds)(b => where (b.id === id) set(b.pricetag := tag))
  }
  
  def setBondFixing(id:String, fixings:String):Unit = transaction{
    update(bonds)(b => where (b.id === id) set(b.fixings := fixings))
  }
  
  def setInitialFXFixing(id:String, initialFX:Double):Unit = transaction{
    update(bonds)(b => where (b.id === id) set(b.initialfx := initialFX))
  }
  
  /**
   * Returns a Set of BondPrice objects identified by a Set of ID.
   * 
   * @param ids A Set of unique IDs.
   * @return A Set of BondPrice objects.
   */
  def getBondPrices:Set[BondPrice] = getKeyedEntity(bondprices)
  def getBondPrices(ids:Traversable[String]):Set[BondPrice] = getKeyedEntity(bondprices, ids)
  
  def getBondPriceByParamSet(paramset:String):Set[BondPrice] = transaction {
      from(bondprices)(b =>
        where(b.paramset === paramset) select(b)).toSet}
  
  def getBondPriceIdByParamSet(paramset:String):Set[String] = transaction {
      from(bondprices)(b =>
        where(b.paramset === paramset) select(&(b.bondid))).toSet}
  
  def getBondPriceByParamDate(paramdate:JavaDate):Set[BondPrice] = transaction {
      from(bondprices)(b =>
        where(b.paramdate === paramdate) select(b)).toSet} 

  def getPricedParamSets:Set[(String, JavaDate)] = transaction {
      from(bondprices)(b => select((&(b.paramset), &(b.paramdate)))).distinct.toSet}
  
  def getPricedParamSets(fromDate:JavaDate, toDate:JavaDate):Set[(String, JavaDate)] = 
    getPricedParamSets.filter{case (pset, pdate) => (pdate >= fromDate && pdate <= toDate)}
  
//  def getNonPricedParamSets:Set[(String, JavaDate)] = transaction {
//      val pricedsets = from(bondprices)(b => select((&(b.paramset), &(b.paramdate)))).distinct.toSet
//      val fullparamset = 
//  }

  def getLatestBondPrices:Option[(String, JavaDate, Set[BondPrice])] = transaction {
      val params:Set[(String, JavaDate)] = from(bondprices)(b => select((&(b.paramset), &(b.paramdate)))).toSet
      params match {
        case p if p.isEmpty => None
        case p => val (maxparam, maxdate) = params.maxBy(_._2)
        		  val prices:Set[BondPrice] = from(bondprices)(b => where(b.paramset === maxparam) select(b)).toSet
        		  Some(maxparam, maxdate, prices)
        }
      }
  
  def getLatestBondPriceIDs:Set[String] = transaction {
      val params:Set[(String, JavaDate)] = from(bondprices)(b => select((&(b.paramset), &(b.paramdate)))).toSet
      params match {
        case p if p.isEmpty => Set.empty
        case p => val (maxparam, maxdate) = params.maxBy(_._2)
        		  from(bondprices)(b => where(b.paramset === maxparam) select(&(b.bondid))).toSet
        }
      }
  
  def getLatestBondPriceParam:Option[(String, JavaDate)] = getPricedParamSets match {
    case s if s.isEmpty => None
    case s => Some(s.maxBy(_._2)) 
  }
  
  def getPricedBondIDs():Set[String] = transaction {
      from(bondprices)(b => select(&(b.bondid))).distinct.toSet
    }
  
  def getPricedBondIDs(paramset:String):Set[String] = transaction {
      from(bondprices)(b => where(b.paramset === paramset) select(&(b.bondid))).distinct.toSet
    }
  
  /**
   * Returns a Set of Volatility objects identified by a Set of ID.
   * 
   * @param ids A Set of unique IDs.
   * @return A Set of Volatility objects.
   */
  def getVolatilities:Set[Volatility] = getKeyedEntity(volatilities)
  def getVolatilities(ids:Traversable[String]):Set[Volatility] = getKeyedEntity(volatilities, ids)

  def getVolatilities(fromDate:JavaDate, toDate:JavaDate):Set[Volatility] = transaction {
      from(volatilities)(vol =>
        where(
            (vol.valuedate gte fromDate) and
            (vol.valuedate lte toDate))
        select(vol)
      ).toSet
    }
  
  def getVolatilityDates:Set[JavaDate] = transaction {
      from(volatilities)(b => select(&(b.valuedate))).distinct.toSet
    }

   def getLatestVolatilityDate:Option[JavaDate] = getVolatilityDates match {
    case d if d.isEmpty => None
    case d => Some(d.maxBy(s => s))
  }
   
  
 def getVolatilityUnderlyings:Set[String] = transaction {
      from(volatilities)(b => select(&(b.underlying))).distinct.toSet
    }
  
  def getVolatilityBondUnderlyings:Set[String] = 
    getVolatilityUnderlyings.withFilter(_.substring(0, 6) == "PRICE:").map(_.substring(6))
    
  def getVolatilityFXUnderlyings:Set[String] = 
    getVolatilityUnderlyings.withFilter(_.substring(0, 3) == "FX:").map(_.substring(3))
  
  /**
   * Returns a Set of Correlation objects identified by a Set of ID.
   * 
   * @param ids A Set of unique IDs.
   * @return A Set of Correlation objects.
   */
  def getCorrelations:Set[Correlation] = getKeyedEntity(correlations)
  def getCorrelations(ids:Traversable[String]):Set[Correlation] = getKeyedEntity(correlations, ids)
  
  def getCorrelationDates:Set[JavaDate] = transaction {
      from(correlations)(b => select(&(b.valuedate))).distinct.toSet
    }
  
  def removeOldCorrelations:Boolean = {
    val correldates = getCorrelationDates
    if (correldates.size > 1) {
      val latest = correldates.max
      transaction {correlations.deleteWhere(b => b.valuedate lt latest)}
      true
    }
    else false
  }
  
  def getLatestCorrelationDate:Option[JavaDate] = getCorrelationDates match {
    case d if d.isEmpty => None
    case d => Some(d.maxBy(s => s))
  }
  
  /**
   * Returns a Set of ForwardPrice objects identified by a Set of ID.
   * 
   * @param ids A Set of unique IDs.
   * @return A Set of ForwardPrice objects.
   */
  def getForwardPrices:Set[ForwardPrice] = getKeyedEntity(forwardprices)
  def getForwardPrices(ids:Traversable[String]):Set[ForwardPrice] = getKeyedEntity(forwardprices, ids)
  
  def getForwardPriceParams:Set[String] = transaction {
      from(forwardprices)(p => select(&(p.paramset))).distinct.toSet
    }
  
  def getForwardPriceDates:Set[JavaDate] = transaction {
      from(forwardprices)(p => select(&(p.paramdate))).distinct.toSet
    }
  
  def getLatestForwardPriceDate:Option[JavaDate] = getForwardPriceDates match {
    case d if d.isEmpty => None
    case d => Some(d.maxBy(s => s))
  } 
  
  
  def removeOldForwardPrices:Boolean = {
    val fwddates = getForwardPriceDates
    if (fwddates.size > 1) {
      val latest = fwddates.max
      transaction {forwardprices.deleteWhere(b => b.valuedate lt latest)}
      true
    }
  else false
   }
  
  /**
   * Returns a Set of ImpliedRate objects identified by a Set of ID.
   * 
   * @param ids A Set of unique IDs.
   * @return A Set of ImpliedRate objects.
   */
  def getImpliedRates:Set[ImpliedRate] = getKeyedEntity(impliedrates)
  def getImpliedRates(ids:Traversable[String]):Set[ImpliedRate] = getKeyedEntity(impliedrates, ids)
  
  def getImpliedRateParams:Set[String] = transaction {
      from(impliedrates)(p => select(&(p.paramset))).distinct.toSet
    }
  
  
  /**
   * Returns a Set of Coupon objects identified by a Set of ID.
   * 
   * @param ids A Set of unique IDs.
   * @return A Set of Coupon objects.
   */
  def getCoupons:Set[Coupon] = getKeyedEntity(coupons)
  def getCoupons(ids:Traversable[String]):Set[Coupon] = getKeyedEntity(coupons, ids)
  
  def getCouponsByBondID(bondid:String):List[Coupon] = getCouponsByBondID(Set(bondid))
  def getCouponsByBondID(bondids:Traversable[String]):List[Coupon] = transaction {
      from(coupons)(c => where(c.bondid in bondids) select(c)).toList
    }
  
  def getUnfixedCoupons(d:JavaDate):List[Coupon] = transaction {
      from(coupons)(c =>
        where((c.eventdate lte d) and 
          (c.rate isNotNull) and
          not (c.rate === "") and
          ((c.fixedrate.isNull) or (c.fixedamount.isNull))
        )
        select(c)
      ).toList
    }
  
  def getDefinedCoupons(d:JavaDate):List[Coupon] = transaction {
      from(coupons)(c =>
        where((c.eventdate lte d) and c.rate.isNotNull and not (c.rate === ""))
        select(c)
      ).toList
    }
  
  def getPreviousCoupons(bondid:String, d:JavaDate):List[Coupon] = transaction {
      from(coupons)(c =>
        where(
            (c.bondid === bondid) and
            (c.enddate lte d) and
            (c.paymenttype === "COUPON"))
        select(c)
      ).toList
    }
  
  def getPreviousCoupons(bondid:String, d:JavaDate, nb:Int):List[Coupon] = {
    val cpns = getPreviousCoupons(bondid, d)
    if (cpns.size <= nb) cpns
    else cpns.toList.sortBy(_.paymentdate).takeRight(nb).toList
  }
  
  def getCurrentCoupons(bondid:String, d:JavaDate):List[Coupon] = transaction {
      from(coupons)(c =>
        where(
            (c.bondid === bondid) and
            (c.startdate lte d) and 
            (c.enddate gt d) and
            (c.paymenttype === "COUPON"))
        select(c)
      ).toList
    }
  
  def getCurrentAndPreviousCoupons(bondid:String, d:JavaDate):(List[Coupon], List[Coupon]) = transaction {
      val current = from(coupons)(c =>
        where(
            (c.bondid === bondid) and
            (c.startdate lte d) and 
            (c.enddate gt d) and
            (c.paymenttype === "COUPON"))
        select(c)
      ).toList
      
      val previous = from(coupons)(c =>
        where(
            (c.bondid === bondid) and
            (c.enddate lte d) and
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
  
  /**
   * Returns list of valid paramsets satisfying the given constraints.
   *
   * @param fromDate: A starting point of Date. Range includes this date.
   * @param toDate: A ending point of Date. Range does includes this date.
  **/
  def getParamSets:Set[(String, JavaDate)] = getRateFXParamSets & getCDSParamSets & getFXParamSets
  
  def getParamSets(fromDate:JavaDate, toDate:JavaDate):Set[(String, JavaDate)] = 
    getParamSets.filter{case (pset, pdate) => (pdate >= fromDate && pdate <= toDate)}
    
  def getParamSetsAfter(basedate:JavaDate):Set[(String, JavaDate)] = 
    getParamSets.filter{case (pset, pdate) => pdate > basedate}
  
  def getLatestParamSet:(String, JavaDate) = getParamSets.maxBy(_._2)
  
  

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
   * @param toDate A ending point of Date. Range includes this date.
   * @param instrument An identifier of the instrument.
   * @param asset An identifier of the asset.
   * @param maturity An identifier of the maturity.
   * @return A List of matching RateFXParameters.
   */
  def getRateFXParameters(fromDate:JavaDate, toDate:JavaDate, instrument:String, asset:String, maturity:String):Set[RateFXParameter] = transaction {
      from(ratefxparameters)(ip =>
        where(
          (ip.paramdate  gte fromDate) and
          (ip.paramdate  lte  toDate) and
          ip.instrument === instrument and
          ip.asset      === asset and
          ip.maturity   === maturity
        )
        select(ip)
      ).toSet
    }
  
  def getRateFXParameters(paramset:String):Set[RateFXParameter] = transaction { 
  	  from(ratefxparameters)(c => 
  	    where(c.paramset === paramset)
  	    select(c)
  	    ).toSet
  	}
  
  
  def getRateFXParameters(on:JavaDate, instrument:String, asset:String, maturity:String):Set[RateFXParameter] = transaction {
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

  def getRateFXParameter(paramset:String, instrument:String, asset:String, maturity:String):Option[Double] = transaction {
      from(ratefxparameters)(ip =>
        where(
          ip.paramset === paramset  and
          ip.instrument === instrument and
          ip.asset      === asset and
          ip.maturity   === maturity
        )
        select(&(ip.value))
      ).headOption
    }
  
  def getRateFXParamSets:Set[(String, JavaDate)] = transaction {
    from(ratefxparameters)(p => select(&(p.paramset), &(p.paramdate))).distinct.toSet
    }
  
  def getLatestRateFXParamsDate:Option[JavaDate] = getRateFXParamSets match {
    case d if d.isEmpty => None
    case d => Some(d.unzip._2.max)
  }
  
  def getEquityParamSets:Set[(String, JavaDate)] = transaction {
    from(ratefxparameters)(p => 
      where(p.instrument === "Equity")
      select(&(p.paramset), &(p.paramdate))).distinct.toSet
    }

  def getLatestEquityParamsDate:Option[JavaDate] = getEquityParamSets match {
    case d if d.isEmpty => None
    case d => Some(d.unzip._2.max)
  }
  
  def getRateFXParams(instrument:String, asset:String):Map[JavaDate, Double] = transaction {
      from(ratefxparameters)(ip =>
        where(
          (ip.paramset like "%-000") and
          ip.instrument === instrument and
          ip.asset      === asset
        )
        select((&(ip.paramdate), &(ip.value)))).toMap
    }
  
  def getRateFXParams(instrument:String, asset:String, maturity:String):Map[JavaDate, Double] = transaction {
      from(ratefxparameters)(ip =>
        where(
          (ip.paramset like "%-000") and
          ip.instrument === instrument and
          ip.asset      === asset and
          ip.maturity   === maturity
        )
        select((&(ip.paramdate), &(ip.value)))).toMap
    }

  def getLatestRateFXParams(instrument:String, asset:String, maturity:String, valuedate:JavaDate):Option[(JavaDate, Double)] = transaction {
      from(ratefxparameters)(ip =>
        where(
          (ip.paramset like "%-000") and
          ip.instrument === instrument and
          ip.asset      === asset and
          ip.maturity   === maturity and
          (ip.paramdate lte valuedate)
        )
        select((&(ip.paramdate), &(ip.value))))
        .reduceOption((p1, p2) => if (p1._1 after p2._1) p1 else p2)
    }
  
  def getLatestRateFXParams(instrument:String, asset:String, valuedate:JavaDate):Option[(JavaDate, Double)] = transaction {
      from(ratefxparameters)(ip =>
        where(
          (ip.paramset like "%-000") and
          ip.instrument === instrument and
          ip.asset      === asset and
          (ip.paramdate lte valuedate)
        )
        select((&(ip.paramdate), &(ip.value))))
        .reduceOption((p1, p2) => if (p1._1 after p2._1) p1 else p2)
	 }

  
  /**
   * Returns historical values of RateFX parameter.
   * Note only the "official" parameters (ie. paramset ending with "-000") is taken.
   *
   * @param fromDate A starting point of Date. Range includes this date.
   * @param toDate A ending point of Date. Range includes this date.
   * @param instrument An identifier of the instrument.
   * @param asset An identifier of the asset.
   * @param maturity An identifier of the maturity.
   * @return time series of the parameter.
   */
  def getRateFXTimeSeries(fromDate:JavaDate, toDate:JavaDate, instrument:String, asset:String, maturity:String):Map[JavaDate, Double] = transaction {
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
  
  def getRateFXTimeSeries(fromDate:JavaDate, toDate:JavaDate, instrument:String, asset:String):Map[JavaDate, Double] = transaction {
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

  
  def getRateFXTimeSeries(instrument:String, asset:String):Map[JavaDate, Double] = transaction {
      from(ratefxparameters)(ip =>
        where(
          (ip.paramset like "%-000") and
          ip.instrument === instrument and
          ip.asset      === asset
        )
        select((&(ip.paramdate), &(ip.value)))
      ).toMap
	 }
  
  def getRateFXTimeSeries(instrument:String, asset:String, maturity:String):Map[JavaDate, Double] = transaction {
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
  
  /**
   * Returns a List of CDSParameters that falls onto a range of Dates.
   *
   * @param fromDate A starting point of Date. Range includes this date.
   * @param toDate A ending point of Date. Range includes this date.
   * @param instrument An identifier of the instrument.
   * @param maturity An identifier of the maturity.
   * @param currencyid An identifier of the currency.
   * @param issuerid An identifier of the issuer.
   * @return A list of matching CDSParameters.
   */
  def getCDSParameters(fromDate:JavaDate, toDate:JavaDate, maturity:String, issuerid:String, currencyid:String, instrument:String):Set[CDSParameter] = transaction {
      from(cdsparameters)(cds =>
        where(
          (cds.paramdate  gte fromDate) and
          (cds.paramdate  lte  toDate) and
          cds.instrument === instrument and
          cds.maturity   === maturity and
          cds.issuerid   === issuerid and
          cds.currencyid === currencyid
        )
        select(cds)
      ).toSet
    }
  
  def getCDSParameters(fromDate:JavaDate, toDate:JavaDate, maturity:String, issuerid:String, currencyid:String):Set[CDSParameter] = transaction {
      from(cdsparameters)(cds =>
        where(
          (cds.paramdate  gte fromDate) and
          (cds.paramdate  lte  toDate) and
          cds.maturity   === maturity and
          cds.issuerid   === issuerid and
          cds.currencyid === currencyid
        )
        select(cds)
      ).toSet
    }

  def getCDSParameters(on:JavaDate, maturity:String, issuerid:String, currencyid:String):Set[CDSParameter] = transaction {
      from(cdsparameters)(cds =>
        where(
          cds.paramdate  === on and
          cds.issuerid   === issuerid and
          cds.currencyid === currencyid
        )
        select(cds)
      ).toSet
    }

  def getCDSParameters(paramset:String):Set[CDSParameter] = transaction {
        from(cdsparameters)(p => where(p.paramset === paramset) select(p)).toSet
    }

  def getCDSParamSets:Set[(String, JavaDate)] = transaction {
    from(cdsparameters)(p => 
//      where (weekday(p.paramdate) < 5)
      select(&(p.paramset), &(p.paramdate))).distinct.toSet
    }


  /**
   * Returns a List of FXParameters that falls onto a range of Dates.
   *
   * @param fromDate A starting point of Date. Range includes this date.
   * @param toDate A ending point of Date. Range includes this date.
   * @param currencyid An identifier of the currency.
   * @return A list of matching FXParameters.
   */
  def getFXParamSets:Set[(String, JavaDate)] = transaction {
    from(fxrates)(p => select(&(p.paramset), &(p.paramdate))).distinct.toSet
    }
  
  def getFXParams(ccy:String):Map[JavaDate, Double] = transaction {
      from(fxrates)(ip =>
        where(
          (ip.paramset like "%-000") and
          ip.currencyid === ccy
        )
        select((&(ip.paramdate), &(ip.fxjpy)))).toMap
    }
  
  def getLatestFXParams(ccy:String, valuedate:JavaDate):Option[(JavaDate, Double)] = transaction {
      from(fxrates)(ip =>
        where(
          (ip.paramset like "%-000") and
          (ip.paramdate lte valuedate) and
          ip.currencyid === ccy
        )
        select((&(ip.paramdate), &(ip.fxjpy))))
        .reduceOption((p1, p2) => if (p1._1 after p2._1) p1 else p2)
    }
  
  def getLatestFXParams(ccy1:String, ccy2:String, valuedate:JavaDate):Option[(JavaDate, Double)] = transaction{
      from(fxrates, fxrates)((fx1, fx2) =>
        where(
          (fx1.paramset like "%-000") and
          (fx1.paramdate lte valuedate) and
//          weekday(fx1.paramdate) < 5 and
          (fx1.paramset === fx2.paramset) and
          (fx1.currencyid === ccy1) and
          (fx2.currencyid === ccy2) 
        )
        select((&(fx1.paramdate), &(fx1.fxjpy), &(fx2.fxjpy))))
        .reduceOption((p1, p2) => if (p1._1 after p2._1) p1 else p2)
      }.map(d => (d._1, d._2/d._3))
      
      
  def getFXParams(ccy1:String, ccy2:String):Map[JavaDate, Double] = transaction{
      from(fxrates, fxrates)((fx1, fx2) =>
        where(
          (fx1.paramset like "%-000") and
//          weekday(fx1.paramdate) < 5 and
          (fx1.paramset === fx2.paramset) and
          (fx1.currencyid === ccy1) and
          (fx2.currencyid === ccy2) 
        )
        select((&(fx1.paramdate), &(fx1.fxjpy), &(fx2.fxjpy))))
      }.map(d => (d._1, d._2/d._3)).toMap
  
  def getFXParameter(ccy:String, paramset:String):Option[Double] = transaction {
      from(fxrates)(ip =>
        where(ip.paramset === paramset and ip.currencyid === ccy)
        select(&(ip.fxjpy))
      ).headOption
    }
      
  def getFXParameter(ccy1:String, ccy2:String, paramset:String):Option[Double] = transaction{
      from(fxrates, fxrates)((fx1, fx2) =>
        where(
          fx1.paramset === paramset and 
          fx2.paramset === paramset and
          fx1.currencyid === ccy1 and 
          fx2.currencyid === ccy2
        )
        select((&(fx1.fxjpy), &(fx2.fxjpy)))).headOption.map(d => d._1 / d._2)
      }
  
  /**
   * Returns historical values of FX spot rate against JPY.
   * Note only the "official" parameters (ie. paramset ending with "-000") is taken.
   *
   * @return fx1 Currency to be measured in JPY.
   */
  def getFXTimeSeries(ccy:String):Map[JavaDate, Double] = transaction {
      from(fxrates)(fx =>
        where(
          (fx.paramset like "%-000") and
          fx.currencyid === ccy
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
  def getFXTimeSeries(currency1:String, currency2:String):Map[JavaDate, Double] = 
	  if (currency2 == "JPY") getFXTimeSeries(currency1)
	  else transaction{
		      from(fxrates, fxrates)((fx1, fx2) =>
		        where(
		          (fx1.paramset like "%-000") and
		          fx1.paramset === fx2.paramset and
		          fx1.currencyid === currency1 and
		          fx2.currencyid === currency2
		        )
		        select((&(fx1.paramdate), &(fx1.fxjpy), &(fx2.fxjpy))))
		        .map(d => (d._1, d._2/d._3))
		      }.toMap
  
  /**
   * Returns historical values of FX spot rate.
   * Note only the "official" parameters (ie. paramset ending with "-000") is taken.
   *
   * @return fx1 Currency to be measured in JPY.
   */
  def getFXTimeSeries(currencylist:Set[String]):Map[String, Map[JavaDate, Double]] = 
	    transaction {
	      currencylist.map( c =>
	        (c, 
		      from(fxrates)(p =>
		        where(
		            (p.paramset like "%-000") and 
		            p.currencyid === c)
		        select((&(p.paramdate), &(p.fxjpy)))
		      ).toMap
		      )).toMap
		 }
  
  
  /**
   * Returns historical values of FX spot rate against JPY.
   * Note only the "official" parameters (ie. paramset ending with "-000") is taken.
   *
   * @param fromDate A starting point of Date. Range includes this date.
   * @param toDate A ending point of Date. Range includes this date.
   * @return fx1 Currency to be measured in JPY.
   */
  def getFXTimeSeries(fromDate:JavaDate, toDate:JavaDate, currency:String):Map[JavaDate, Double] =
    transaction {
      from(fxrates)(fx =>
        where(
          (fx.paramset like "%-000") and
          fx.currencyid === currency and
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
   * @param toDate A ending point of Date. Range includes this date.
   * @return fx1 Currency to be measured.
   * @return fx2 Measuring currency.
   * 			 For example, fx1 = "USD" & fx2 = "JPY" returns USD/JPY spot rate = around 80.00
   */
  def getFXTimeSeries(fromDate:JavaDate, toDate:JavaDate, currency1:String, currency2:String):Map[JavaDate, Double] = 
	  if (currency2 == "JPY") getFXTimeSeries(fromDate, toDate, currency1)
	  else transaction{
		      from(fxrates, fxrates)((fx1, fx2) =>
		        where(
		          (fx1.paramset like "%-000") and
		          fx1.paramset === fx2.paramset and
		          (fx1.paramdate gte fromDate) and
		          (fx1.paramdate lte toDate) and
		          fx1.currencyid === currency1 and
		          fx2.currencyid === currency2
		        )
		        select((&(fx1.paramdate), &(fx1.fxjpy), &(fx2.fxjpy))))
		        .map(d => (d._1, d._2/d._3))
		      }.toMap

  /**
   * Returns list of all currencies in the FX database (against JPY).
   */
  def getFXlist:Set[String] = transaction {
    from(fxrates)(fx => select(&(fx.currencyid))).distinct.toSet
    }
		      
  /**
   * Returns historical values of CDS.
   * Note only the "official" parameters (ie. paramset ending with "-000") is taken.
   *
   * @param fromDate A starting point of Date. Range includes this date.
   * @param toDate A ending point of Date. Range includes this date.
   * @return currencyid CDS quotation currency. eg. "USD"
   * @return issuerid Issuer id. eg. "ADB"
   * @return maturity CDS maturity. eg. "6M" or "5Y"
   */
  def getCDSTimeSeries(fromDate:JavaDate, toDate:JavaDate, currencyid:String, issuerid:String, maturity:String):Map[JavaDate, Double] = transaction {
      from(cdsparameters)(ip =>
        where(
          (ip.paramdate gte fromDate) and
          (ip.paramdate lte toDate) and
          (ip.paramset like "%-000") and
          ip.issuerid === issuerid and
          ip.currencyid === currencyid and
          ip.maturity === maturity
        )
        select((&(ip.paramdate), &(ip.spread)))
      ) toMap
    }
  
  /**
   * Returns historical price of a bond, quoted in issue currency.
   * Note only the "official" parameters (ie. paramset ending with "-000") is taken.
   *
   * @param fromDate A starting point of Date. Range includes this date.
   * @param toDate A ending point of Date. Range includes this date.
   * @return bondid Bond id. eg. "ADB-00001"
   */
  def getPriceTimeSeries(bondid:String):Map[JavaDate, Double] = transaction {
      from(bondprices)(bp =>
        where(
          (bp.paramset like "%-000") and
//          bp.instrument === "BONDPRICE" and
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
   * @param toDate A ending point of Date. Range includes this date.
   * @return bondid Bond id. eg. "ADB-00001"
   */
  def getPriceTimeSeries(start:JavaDate, end:JavaDate, bondid:String):Map[JavaDate, Double] = transaction {
      from(bondprices)(bp =>
        where(
          (bp.paramdate gte start) and
          (bp.paramdate lte end) and
          (bp.paramset like "%-000") and
//          bp.instrument === "BONDPRICE" and
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
   * @param toDate A ending point of Date. Range includes this date.
   * @param bondid target bond id
   * @param defaultfx default fx value in case JPY price is not available for all currencies
   * @return bondid Bond id. eg. "ADB-00001"
   */
  def getJPYPriceTimeSeries(start:JavaDate, end:JavaDate, bondid:String):Map[JavaDate, Double] = transaction {
      from(bondprices)(bp =>
        where(
          (bp.paramdate gte start) and
          (bp.paramdate lte end) and
          (bp.paramset like "%-000") and
//          bp.instrument === "BONDPRICE" and
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
  def getJPYPriceTimeSeries(bondid:String, basefx:Double):Map[JavaDate, Double] = transaction {
      val quoteccy = from(bonds)(b => 
        where (b.id === bondid)
        select (&(b.currencyid))
        ).singleOption.orNull
        
      if (quoteccy == null) Map.empty
      else from(bondprices, fxrates)((bp, fx) =>
	        where(
	          bp.paramset like "%-000" and
//	          bp.instrument === "BONDPRICE" and
	          bp.bondid      === bondid and
	          bp.priceclean.isNotNull and
	          fx.currencyid === quoteccy and
	          bp.paramset === fx.paramset
	        )
	        select(&(bp.paramdate), &(bp.priceclean.get), &(fx.fxjpy))
	      ).map(v => (v._1, v._2 * v._3 / basefx)).toMap
  	}
  
  
  
  /**
   * Returns historical price of a bond, quoted as JPY percentage value from given fx fixing.
   * Note only the "official" parameters (ie. paramset ending with "-000") is taken.
   * It returns prices for dates which price in original currency is defined.
   *
   * @param fromDate A starting point of Date. Range includes this date.
   * @param toDate A ending point of Date. Range includes this date.
   * @param bondid target bond id
   * @param defaultfx default fx value in case JPY price is not available for all currencies
   * @param fx	
   * @return bondid Bond id. eg. "ADB-00001"
   */
  def getJPYPriceTimeSeries(start:JavaDate, end:JavaDate, bondid:String, basefx:Double):Map[JavaDate, Double] = transaction {
      val quoteccy = from(bonds)(b => 
        where (b.id === bondid)
        select (&(b.currencyid))
        ).singleOption.orNull
        
      if (quoteccy == null) Map.empty
      else from(bondprices, fxrates)((bp, fx) =>
	        where(
	          bp.paramset like "%-000" and
//	          bp.instrument === "BONDPRICE" and
	          (bp.paramdate gte start) and
	          (bp.paramdate lte end) and
	          bp.bondid      === bondid and
	          bp.priceclean.isNotNull and
	          fx.currencyid === quoteccy and
	          bp.paramset === fx.paramset
	        )
	        select(&(bp.paramdate), &(bp.priceclean.get), &(fx.fxjpy))
	      ).map(v => (v._1, v._2 * v._3 / basefx)).toMap
  	}
  
  def insertStringEntity[T <: KeyedEntity[String]](data:T):Unit = transaction{
    dataTable(data.getClass.getSimpleName.toString) match {
      case Some(t:Table[T]) => t.insert(data)
      case _ => println("table not found")
    }
  }
  
  def insertStringEntity[T <: KeyedEntity[String]](data:Set[T]):Unit = transaction{
    dataTable(data.head.getClass.getSimpleName.toString) match {
      case Some(t:Table[T]) => t.insert(data)
      case _ => println("table not found")
    }
  }
  
  def updateStringEntity[T <: KeyedEntity[String]](data:Set[T]):Unit = transaction{
    dataTable(data.head.getClass.getSimpleName.toString) match {
      case Some(t:Table[T]) => t.update(data)
      case _ => println("table not found")
    }
  }
  
  def updateStringEntity[T <: KeyedEntity[String]](data:T):Unit = transaction{
//	  Session.currentSession.setLogger(msg => println(msg))    
    dataTable(data.getClass.getSimpleName.toString) match {
      case Some(t:Table[T]) => t.update(data)
      case _ => println("table not found")
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
    
//    val datatable:Table[_ <: KeyedEntity[String]] = data.head.getClass.getSimpleName.toString match {
//      case "BondPrice" => bondprices
//      case "Volatility" => volatilities
//      case "Correlation" => correlations
//      case "Coupon" => coupons
//      case "ForwardPrice" => forwardprices
//      case "ImpliedRate" => impliedrates
//      case _ => null
//    }
    
    dataTable(data.head.getClass.getSimpleName.toString) match {
      case Some(t) => insertMany(data.toSet, overwrite)
      case None => 0
    }
    
//    if (datatable == null) return 0
//    insertMany(data.toSet, overwrite)
  }
  
  def dataTable(name:String):Option[Table[_ <: KeyedEntity[String]]] = name match {
      case "BondPrice" => Some(bondprices)
      case "Volatility" => Some(volatilities)
      case "Correlation" => Some(correlations)
      case "Coupon" => Some(coupons)
      case "ForwardPrice" => Some(forwardprices)
      case "ImpliedRate" => Some(impliedrates)
      case "Bond" => Some(bonds)
      case _ => None
    }
  
  def insertOrUpdate[T <: KeyedEntity[Int]](data:Traversable[T], overwrite:Boolean)(implicit d:DummyImplicit):Int = {
    
    if (data.isEmpty) return 0
    
    val datatable = data.head.getClass.getSimpleName.toString match {
      case "InputParameter" => inputparameters
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
    runSQLCommitStatement
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
 //   if (dataSource.getNumBusyConnections > 1) session.close
    session.close
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
//    if (dataSource.getNumBusyConnections > 2) session.close
    session.close
    result
  }
  
  def runSQLCommitStatement:Int = {
    val session           = SessionFactory.concreteFactory.get()
    val preparedStatement = session.connection.prepareStatement("COMMIT;")
    val result = preparedStatement.executeUpdate
//    if (dataSource.getNumBusyConnections > 2) session.close
    session.close
    result
  }

  /**
   * Builds MySQL CSV import statement from multiple Squeryl objects.
   * CSV file for the objects will be automatically generated, and embedded in INFILE-clause in the query.
   *
   * @param objects List of a Squeryl objects of a same Model, such as List[BondPrice]
   * @return A List of strings of prepared SQL statement.
   */
  def buildCSVImportStatement(objects:Set[AnyRef], overwrite:Boolean, batchsize:Int = 50000):List[String] =  {
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
	    val out = new java.io.OutputStreamWriter(new java.io.FileOutputStream(tempFile), "UTF-8")
	    out.write(builder.toString)
	    out.close()
	    tempFilePath
    }).toList
    
    val insertstatement = tempfiles.map(f => 
      "LOAD DATA LOCAL INFILE '" + f.replaceAll("\\\\", "\\\\\\\\") + "' " +
        (if(overwrite) "REPLACE " else "IGNORE ") + "INTO TABLE " + tableName + " " +
        "FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '''' " +
        "IGNORE 1 LINES " +
        "(" + columnNames.map(n => "`" + n + "`").mkString(", ") + ")" + ";"
    )
    
    
    List("START TRANSACTION;", "SET FOREIGN_KEY_CHECKS = 1;") ++ insertstatement ++ List("COMMIT;")
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
