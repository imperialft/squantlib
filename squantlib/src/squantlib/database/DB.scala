package squantlib.database

import java.sql.{Timestamp, Time, DriverManager}
import com.mysql.jdbc.Driver
import com.mchange.v2.c3p0._
import org.squeryl.dsl.{TypedExpression, TDate, TInt, TypedExpressionFactory, QueryDsl}
import org.squeryl.dsl.ast.FunctionNode
import org.squeryl.adapters._
import org.squeryl.{Session, SessionFactory, Schema, Table}
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.{KeyedEntityDef, KeyedEntity, Table}
import squantlib.database.schemadefinitions._
import scala.collection.mutable.{MutableList, StringBuilder}
import java.io.{File, FileWriter, BufferedWriter, FileInputStream}
import java.util.{Date => JavaDate, Calendar => JavaCalendar, UUID, Properties}
import java.text.SimpleDateFormat
import org.apache.commons.lang3.StringEscapeUtils
import com.sun.org.apache.xpath.internal.operations.And
import squantlib.util.JsonUtils
import squantlib.util.JsonUtils._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.node.ObjectNode
  

object DB extends Schema { 
  
 implicit object StringDataKED extends KeyedEntityDef[StringEntity, String] {
   def getId(a: StringEntity) = a.id
   def isPersisted(a: StringEntity) = a.id != null
   def idPropertyName = "id"
 }  
  
 implicit object IntDataKED extends KeyedEntityDef[IntEntity, Int] {
   def getId(a: IntEntity) = a.id
   def isPersisted(a: IntEntity) = a.id > 0
   def idPropertyName = "id"
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
  val equities = table[Equity]("Equities")
  val bondprices = table[BondPrice]("BondPrices")
  val latestprices = table[LatestPrice]("LatestPrices")
  val historicalprices = table[HistoricalPrice]("HistoricalPrices")
  val volatilities = table[Volatility]("Volatilities")
  val correlations = table[Correlation]("Correlations")
  val coupons = table[Coupon]("Coupons")
  val forwardprices = table[ForwardPrice]("ForwardPrices")
  val impliedrates = table[ImpliedRate]("ImpliedRates")
  val underlyings = table[Underlying]("Underlyings")
  val jsdaprices = table[JsdaPrice]("JSDAPrice")
  
  val fxrates = table[FXRate]("FXRates")
  val ratefxparameters = table[RateFXParameter]("RateFXParameters")
  val inputparameters = table[InputParameter]("InputParameters")
  val cdsparameters = table[CDSParameter]("CDSParameters")
  val distributorbranches = table[DistributorBranch]("DistributorBranches")

  private def getKeyedEntity[A<:StringEntity](t:Table[A]):Set[A] = transaction {
      from(t)(p => select(p)).toSet}
  
  private def getKeyedEntity[A<:StringEntity](t:Table[A], ids:Traversable[String]):Set[A] = transaction {
      from(t)(p => where(p.id in ids) select(p)).toSet }
  
  private def getAKeyedEntity[A<:StringEntity](t:Table[A], id:String):Option[A] = transaction {
      from(t)(p => where(p.id === id) select(p)).headOption }
  
  private def getKeyedIntEntity[A<:IntEntity](t:Table[A]):Set[A] = transaction {
      from(t)(p => select(p)).toSet}
  
  private def getKeyedIntEntity[A<:IntEntity](t:Table[A], ids:Traversable[Int]):Set[A] = transaction {
      from(t)(p => where(p.id in ids) select(p)).toSet }
  
  private def getAKeyedIntEntity[A<:IntEntity](t:Table[A], id:Int):Option[A] = transaction {
      from(t)(p => where(p.id === id) select(p)).headOption }
  
  private def weekday(b: TypedExpression[java.util.Date,TDate])
  (implicit f: TypedExpressionFactory[Int,TInt]) = f.convert(new FunctionNode("WEEKDAY", Seq(b)))
  
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
  def getCountry(id:String):Option[Country] = getAKeyedEntity(countries, id)

  /**
   * Returns a Set of equity description objects identified by a List of ID.
   * 
   * @param ids A List of unique IDs.
   * @return A Set of Equity objects.
   */
  def getEquities:Set[Equity] = getKeyedEntity(equities)
  def getEquities(ids:Traversable[String]):Set[Equity] = getKeyedEntity(equities, ids)
  def getEquity(id:String):Option[Equity] = getAKeyedEntity(equities, id)
  
  /**
   * Returns a Set of Currencies objects identified by a Set of ID.
   * 
   * @param ids A Set of unique IDs.
   * @return A Set of Currency objects.
   */
  def getCurrencies:Set[Currency] = getKeyedEntity(currencies)
  def getCurrencies(ids:Traversable[String]):Set[Currency] = getKeyedEntity(currencies, ids)
  def getCurrency(id:String):Option[Currency] = getAKeyedEntity(currencies, id)
  
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
  def getDistributor(id:String):Option[Distributor] = getAKeyedEntity(distributors, id)
  
  /**
   * Returns a Set of DistributorBranch objects identified by a Set of ID.
   * 
   * @param ids A Set of unique IDs.
   * @return A Set of Distributor objects.
   */
  def getDistributorBranches:Set[DistributorBranch] = getKeyedIntEntity(distributorbranches)
  def getDistributorBranches(ids:Traversable[Int]):Set[DistributorBranch] = getKeyedIntEntity(distributorbranches, ids)
  def getDistributorBranch(id:Int):Option[DistributorBranch] = getAKeyedIntEntity(distributorbranches, id)

  /**
   * Returns a Set of Issuer objects identified by a Set of ID.
   * 
   * @param ids A Set of unique IDs.
   * @return A Set of Issuer objects.
   */
  def getIssuers:Set[Issuer] = getKeyedEntity(issuers)
  def getIssuers(ids:Traversable[String]):Set[Issuer] = getKeyedEntity(issuers, ids)
  def getIssuer(id:String):Option[Issuer] = getAKeyedEntity(issuers, id)
  
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
  def getProduct(id:String):Option[Product] = getAKeyedEntity(products, id)
  
  /**
   * Returns a Set of Bond objects identified by a Set of ID.
   * 
   * @param ids A Set of unique IDs.
   * @return A Set of Bond objects.
   */

  def getBonds:Set[Bond] = getKeyedEntity(bonds)
  def getBonds(ids:Traversable[String]):Set[Bond] = getKeyedEntity(bonds, ids)
  def getBond(id:String):Option[Bond] = getAKeyedEntity(bonds, id)
  
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
  
  def setBondPriceType(id:String, pricetype:String):Unit = transaction{
    update(bonds)(b => where (b.id === id) set(b.pricetype := pricetype))
  }
  
  
  def setBondPriceType(ids:Set[String], pricetype:String):Unit = transaction{
    update(bonds)(b => where (b.id in ids) set(b.pricetype := pricetype))
  }
  
  def setBondFixing(id:String, fixings:String):Unit = transaction{
    update(bonds)(b => where (b.id === id) set(b.fixings := fixings))
  }
  
  def setInitialFXFixing(id:String, initialFX:Double):Unit = transaction{
    update(bonds)(b => where (b.id === id) set(b.initialfx := initialFX))
  }
  
  def getBondSetting(id:String):String = transaction{
    from(bonds)(b => where (b.id === id) select (&(b.settings))).headOption.getOrElse(null)
  }
  
  def getBondSettingJson(id:String):ObjectNode = {
    val currentsetting = getBondSetting(id)
    if (currentsetting != null && !currentsetting.isEmpty) currentsetting.objectNode.getOrElse(JsonUtils.newObjectNode)
    else JsonUtils.newObjectNode
  }
  
  def setBondSetting(id:String, setting:String):Unit = transaction{
    update(bonds)(b => where (b.id === id) set(b.settings := setting))
  }
  
  def updateBondSetting[T<:AnyVal](id:String, name:String, newvalue:T):Unit = updateBondSetting(id, name, newvalue.toString)
  
  def updateBondSetting(id:String, name:String, newvalue:String):Unit = {
    val currentjson = getBondSettingJson(id)
    currentjson.put(name, newvalue)
    setBondSetting(id, currentjson.toJsonString)
  }
  
  def updateBondSetting(id:String, name:String, newnode:JsonNode):Unit = {
    val currentjson = getBondSettingJson(id)
    currentjson.put(name, newnode)
    setBondSetting(id, currentjson.toJsonString)
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
  
  def getBondPriceCount:Map[String, Int] = transaction {
      from(bondprices)(b =>
        groupBy(b.bondid) compute(count(b.id))).map(c => (c.key, c.measures.toInt)).toMap
  }
  
  /*
   * Added for LatestPrice
   */
  
  def getLatestPrices:Set[LatestPrice] = getKeyedEntity(latestprices)
  def getLatestPrices(ids:Traversable[String]):Set[LatestPrice] = getKeyedEntity(latestprices, ids)
  
  def getLatestPriceParam:(String, JavaDate) = {
    val paramsets = transaction {from(latestprices)(b => select((&(b.paramset), &(b.paramdate)))).distinct.toSet}
    paramsets.maxBy(_._1)
  }
  
  def getHistoricalPriceCount:Map[String, Int] = transaction {
    from(historicalprices)(b =>
      groupBy(b.bondid) compute(count(b.id))).map(c => (c.key, c.measures.toInt)).toMap
  }
  
  def getLatestHistoricalPrice:Option[JavaDate] = transaction {
    from(historicalprices)(b => compute(max(b.paramdate))).map(c => c)
  }
  
  def getHistoricalPriceDates:Set[JavaDate] = transaction {
    from(historicalprices)(b => select(&(b.paramdate))).distinct.toSet
  }
  
  def getHistoricalPrices(d:JavaDate):Set[HistoricalPrice] = transaction {
    from(historicalprices)(b => where(b.paramdate === d) select(b)).toSet
  }
  
  def getHistoricalPriceIDs(d:JavaDate):Set[String] = transaction {
    from(historicalprices)(b => where(b.paramdate === d) select(&(b.bondid))).toSet
  }
  
  def removeHistoricalPrice(bondid:String) = transaction {
    bondprices.deleteWhere(b => b.bondid === bondid)
    historicalprices.deleteWhere(b => b.bondid === bondid)
    latestprices.deleteWhere(b => b.bondid === bondid)
  }
  
  def getLatestBondPriceIDs:Set[String] = transaction {
    val maxparam:Option[JavaDate] = from(bondprices) (b => compute(max(b.paramdate)))
    maxparam match {
      case None => Set.empty
      case Some(d) => from (bondprices) (b => where(b.paramdate === d) select (&(b.bondid))).toSet
    }
  }
  
  def getLatestBondPrices:Set[BondPrice] = transaction {
    val maxparam:Option[JavaDate] = from(bondprices) (b => compute(max(b.paramdate)))
    maxparam match {
      case None => Set.empty
      case Some(d) => from (bondprices) (b => where(b.paramdate === d) select (b)).toSet
    }
  }
  
  def getLatestBondPrice(bondid:String):Option[BondPrice] = transaction {
    from (bondprices) (b => 
      where (b.bondid === bondid) 
      select(b)
      orderBy(b.paramdate desc)
      ).headOption
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
    
  def removeOldVolatilities(nbDays:Int):Boolean = {
    val voldates = getVolatilityDates.toList.sorted
    if (voldates.size > nbDays) {
      val startdate = voldates.takeRight(nbDays).min
      transaction {volatilities.deleteWhere(b => b.valuedate lt startdate)}
      true
    }
    else false
  }
    
  
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
  
  def removeOldCorrelations(nbDays:Int):Boolean = {
    val correldates = getCorrelationDates.toList.sorted
    if (correldates.size > nbDays) {
      val startdate = correldates.takeRight(nbDays).min
      transaction {volatilities.deleteWhere(b => b.valuedate lt startdate)}
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
      transaction {forwardprices.deleteWhere(b => b.paramdate lt latest)}
      true
    } else false
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
  def getParamSets:Set[(String, JavaDate)] = {
    val params = Map(
        "ratefx" -> getRateFXParamSets,
        "cds" -> getCDSParamSets,
        "fx" -> getFXParamSets)
    params.foreach{case (n, v) => println(n + " : " + (if(v.isEmpty) "not found" else v.maxBy(_._2)))}
    params.values.reduce(_ & _)
  }
  
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
          (ip.paramdate gte fromDate) and
          (ip.paramdate lte toDate) and
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
  
  def getHistoricalRateFX(instrument:String, asset:String):Map[JavaDate, Double] = getHistoricalRateFX(instrument, asset, null, null)
  
  def getHistoricalRateFX(instrument:String, asset:String, start:JavaDate, end:JavaDate):Map[JavaDate, Double] = 
    if (start != null && end != null)
      transaction { 
        from(ratefxparameters)(ip =>
        where(
          (ip.paramset like "%-000") and
          ip.instrument === instrument and
          ip.asset      === asset and
          (ip.paramdate gte start) and
          (ip.paramdate lte end)
        )
        select((&(ip.paramdate), &(ip.value)))).toMap
    }
    else
      transaction { from(ratefxparameters)(ip =>
        where(
          (ip.paramset like "%-000") and
          ip.instrument === instrument and
          ip.asset      === asset
        )
        select((&(ip.paramdate), &(ip.value)))).toMap
    }  
  
  def getHistoricalRateFX(instrument:String, asset:String, maturity:String):Map[JavaDate, Double] = getHistoricalRateFX(instrument, asset, maturity, null, null)
  
  def getHistoricalRateFX(instrument:String, asset:String, maturity:String, start:JavaDate, end:JavaDate):Map[JavaDate, Double] = 
    if (start != null && end != null)
    transaction {
      from(ratefxparameters)(ip =>
        where(
          (ip.paramset like "%-000") and
          ip.instrument === instrument and
          ip.asset      === asset and
          ip.maturity   === maturity and
          (ip.paramdate gte start) and
          (ip.paramdate lte end)
        )
        select((&(ip.paramdate), &(ip.value)))).toMap
    }
    else
    transaction {
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
  
  def getLatestFXParamSet:Option[(String, JavaDate)] = getFXParamSets match {
    case s if s.isEmpty => None
    case s => Some(s.maxBy(_._2))
  }
  
  def getFXParams(ccy:String):Map[JavaDate, Double] = transaction {
      from(fxrates)(ip =>
        where((ip.paramset like "%-000") and ip.currencyid === ccy)
        select((&(ip.paramdate), &(ip.fxjpy)))).toMap
    }
  
  def getFXParams(ccy:String, start:JavaDate, end:JavaDate):Map[JavaDate, Double] = 
    if (start != null && end != null) 
      transaction {
        from(fxrates)(ip =>
        where(
          (ip.paramset like "%-000") and
          ip.currencyid === ccy and
          (ip.paramdate gte start) and
          (ip.paramdate lte end)
        )
        select((&(ip.paramdate), &(ip.fxjpy)))).toMap
      }
    else
      transaction {
        from(fxrates)(ip =>
        where(
          (ip.paramset like "%-000") and
          ip.currencyid === ccy
        )
        select((&(ip.paramdate), &(ip.fxjpy)))).toMap
      }
      
  
  def getFXParams(ccy1:String, ccy2:String):Map[JavaDate, Double] = getFXParams(ccy1, ccy2, null, null)
  
  def getFXParams(ccy1:String, ccy2:String, start:JavaDate, end:JavaDate):Map[JavaDate, Double] = 
    if (start != null && end != null) 
      transaction{from(fxrates, fxrates)((fx1, fx2) =>
        where(
          (fx1.paramset like "%-000") and
          (fx1.paramset === fx2.paramset) and
          (fx1.currencyid === ccy1) and
          (fx2.currencyid === ccy2) and
          (fx1.paramdate gte start) and
          (fx1.paramdate lte end)
        )
        select((&(fx1.paramdate), &(fx1.fxjpy), &(fx2.fxjpy))))
      }.map(d => (d._1, d._2/d._3)).toMap
    else
      transaction{from(fxrates, fxrates)((fx1, fx2) =>
        where(
          (fx1.paramset like "%-000") and
          (fx1.paramset === fx2.paramset) and
          (fx1.currencyid === ccy1) and
          (fx2.currencyid === ccy2) 
        )
        select((&(fx1.paramdate), &(fx1.fxjpy), &(fx2.fxjpy))))
      }.map(d => (d._1, d._2/d._3)).toMap
      
  def getLatestFX(ccy:String):Option[(JavaDate, Double)] = transaction {
        val maxdate = from (fxrates)(fx => 
          where((fx.paramset like "%-000") and (fx.currencyid === "AUD")) 
          compute(max(fx.paramdate)))
          
        from (fxrates)(fx => 
          where((fx.paramset like "%-000") and (fx.currencyid === "AUD") and (fx.paramdate === maxdate)) 
          select ((&(fx.paramdate), &(fx.fxjpy))))}.headOption

  
  def getLatestFX(ccy:String, valuedate:JavaDate):Option[(JavaDate, Double)] = transaction {
      from(fxrates)(ip =>
        where(
          (ip.paramset like "%-000") and
          (ip.paramdate lte valuedate) and
          ip.currencyid === ccy
        )
        select((&(ip.paramdate), &(ip.fxjpy))))
        .reduceOption((p1, p2) => if (p1._1 after p2._1) p1 else p2)
    }
  
  def getLatestFX(ccy1:String, ccy2:String, valuedate:JavaDate):Option[(JavaDate, Double)] = transaction{
      from(fxrates, fxrates)((fx1, fx2) =>
        where(
          (fx1.paramset like "%-000") and
          (fx1.paramdate lte valuedate) and
          (fx1.paramset === fx2.paramset) and
          (fx1.currencyid === ccy1) and
          (fx2.currencyid === ccy2) 
        )
        select((&(fx1.paramdate), &(fx1.fxjpy), &(fx2.fxjpy))))
        .reduceOption((p1, p2) => if (p1._1 after p2._1) p1 else p2)
      }.map(d => (d._1, d._2/d._3))
      
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
  def getBondPriceTimeSeries(bondid:String):Map[JavaDate, Double] = transaction {
      from(bondprices)(bp =>
        where(
          (bp.paramset like "%-000") and
          bp.bondid      === bondid and
          bp.priceclean.isNotNull
        )
        select(&(bp.paramdate), &(bp.priceclean.get))
      ) toMap;
  	}
  
  /**
   * Returns forward price of an asset, quoted in issue currency.
   */
  def getForwardPricesTimeSeries(underlyingtype:String, underlyingname:String):Map[JavaDate, Double] = transaction {
      from(forwardprices)(fp =>
        where(
          fp.underlyingtype === underlyingtype and
          fp.underlyingname === underlyingname
        )
        select(&(fp.valuedate), &(fp.price))
      ) toMap;
  	}
  
  def getForwardPrices(underlyingtype:String, underlyingname:String):Set[ForwardPrice] = transaction {
      from(forwardprices)(fp =>
        where(
          fp.underlyingtype === underlyingtype and
          fp.underlyingname === underlyingname
        )
        select(fp)
      ) toSet;
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
          bp.bondid      === bondid and
          bp.priceclean.isNotNull
        )
        select(&(bp.paramdate), &(bp.priceclean.get))
      ) toMap;
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
  def getJPYPriceTimeSeries(bondid:String):Map[JavaDate, Double] = {
    val bond = getBond(bondid).orNull
    
    if (bond == null) Map.empty

    else {
      
      val results = transaction{from(bondprices)(bp =>
        where(
          bp.paramset like "%-000" and
	      bp.bondid      === bondid and
	      bp.priceclean.isNotNull
	    )
	    select(&(bp.paramdate), &(bp.priceclean.get), &(bp.fxjpy))
	  ).toSet}
	  
	  val basefx = if (bond.initialfx > 0) bond.initialfx else results.maxBy{case (d, p, f) => d}._3
	  
	  results.map(v => (v._1, v._2 * v._3 / basefx)).toMap
  }}
  
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
    from(bondprices)(bp =>
      where(
          bp.paramset like "%-000" and
	      bp.bondid      === bondid and
	      bp.priceclean.isNotNull
	      )
	  select(&(bp.paramdate), &(bp.priceclean.get), &(bp.fxjpy))
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
  
  def getHistoricalJsdaPrice(bondid:String, start:JavaDate, end:JavaDate):Map[JavaDate, Double] = 
    if (start != null && end != null)
      transaction { 
        from(jsdaprices)(p =>
        where(
          p.bondid === bondid and
          (p.paramdate gte start) and
          (p.paramdate lte end)
        )
        select((&(p.paramdate), &(p.price)))).toMap
    }
    else
      transaction { from(jsdaprices)(p =>
        where(p.bondid === bondid)
        select((&(p.paramdate), &(p.price)))).toMap
    }
  
  
  def insertStringEntity[T <: StringEntity](data:T):Unit = transaction{
    dataTable(data.getClass.getSimpleName.toString) match {
      case Some(t:Table[T]) => t.insert(data)
      case _ => println("table not found")
    }
  }
  
  def insertStringEntity[T <: StringEntity](data:Set[T]):Unit = transaction{
    dataTable(data.head.getClass.getSimpleName.toString) match {
      case Some(t:Table[T]) => t.insert(data)
      case _ => println("table not found")
    }
  }
  
  def updateStringEntity[T<:StringEntity](data:Set[T]):Unit = transaction{
    dataTable(data.head.getClass.getSimpleName.toString) match {
      case Some(t:Table[T]) => t.update(data)
      case _ => println("table not found")
    }
  }
  
  def insertOrUpdateStringEntity[T<:StringEntity](data:T):Unit = transaction{
    dataTable(data.getClass.getSimpleName.toString) match {
      case Some(t:Table[T]) => t.insertOrUpdate(data)
      case _ => println("table not found")
    }
  }
  
  def updateStringEntity[T<:StringEntity](data:T):Unit = transaction{
    dataTable(data.getClass.getSimpleName.toString) match {
      case Some(t:Table[T]) => t.update(data)
      case _ => println("table not found")
    }
  }

  def insertIntEntity[T <: IntEntity](data:T):Unit = transaction{
    intTable(data.getClass.getSimpleName.toString) match {
      case Some(t:Table[T]) => t.insert(data)
      case _ => println("table not found")
    }
  }
  
  def insertIntEntity[T<:IntEntity](data:Set[T]):Unit = transaction{
    intTable(data.head.getClass.getSimpleName.toString) match {
      case Some(t:Table[T]) => t.insert(data)
      case _ => println("table not found")
    }
  }
  
  def updateIntEntity[T<:IntEntity](data:Set[T]):Unit = transaction{
    intTable(data.head.getClass.getSimpleName.toString) match {
      case Some(t:Table[T]) => t.update(data)
      case _ => println("table not found")
    }
  }
  
  def updateIntEntity[T<:IntEntity](data:T):Unit = transaction{
    intTable(data.getClass.getSimpleName.toString) match {
      case Some(t:Table[T]) => t.update(data)
      case _ => println("table not found")
    }
  }  
  
  def dataTable(name:String):Option[Table[_ <: StringEntity]] = name match {
      case "BondPrice" => Some(bondprices)
      case "LatestPrice" => Some(latestprices)
      case "HistoricalPrice" => Some(historicalprices)
      case "Volatility" => Some(volatilities)
      case "Correlation" => Some(correlations)
      case "Coupon" => Some(coupons)
      case "ForwardPrice" => Some(forwardprices)
      case "ImpliedRate" => Some(impliedrates)
      case "Bond" => Some(bonds)
      case "Currency" => Some(currencies)
      case "Issuer" => Some(issuers)
      case "Product" => Some(products)
      case _ => None
    }
  
  def intTable(name:String):Option[Table[_ <: IntEntity]] = name match {
      case "RateFXParameter" => Some(ratefxparameters)
      case "InputParameter" => Some(inputparameters)
      case "DistributorBranch" => Some(distributorbranches)
      case "FXRate" => Some(fxrates)
      case "CDSParameter" => Some(cdsparameters)
      case _ => None
    }
  
  /**
   * Inserts bond prices to the database.
   *
   * @param objects A List of Squeryl model objects.
   * @return Whether or not the statement ran successfully.
   *          However, this does not guarantee whether every row has been inserted.
   */
  def insertOrUpdate[T <: StringEntity](data:Traversable[T], overwrite:Boolean):Int = {
    if (data.isEmpty) return 0
    dataTable(data.head.getClass.getSimpleName.toString) match {
      case Some(t) => insertMany(data.toSet, overwrite)
      case None => 0
    }
  }
  
  def insertOrUpdate[T <: IntEntity](data:Traversable[T], overwrite:Boolean)(implicit d:DummyImplicit):Int = {
    if (data.isEmpty) return 0
    val datatable = data.head.getClass.getSimpleName.toString match {
      case "InputParameter" => inputparameters
      case "FXRate" => fxrates
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
  def empty[T](table:Table[T]):Boolean = {
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
    session.close
    result
  }
  
  def runSQLStatements(statements:List[String]):Array[Int] = {
    val session           = SessionFactory.concreteFactory.get()
    val statement = session.connection.createStatement
    statements.foreach(s => statement.addBatch(s))
    val result = statement.executeBatch
    session.close
    result
  }
  
  def runSQLCommitStatement:Int = {
    val session           = SessionFactory.concreteFactory.get()
    val preparedStatement = session.connection.prepareStatement("COMMIT;")
    val result = preparedStatement.executeUpdate
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
