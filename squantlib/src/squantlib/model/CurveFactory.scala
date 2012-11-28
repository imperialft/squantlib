package squantlib.model

import scala.collection.mutable.HashMap
import scala.collection.JavaConversions
import squantlib.model.yieldparameter.{YieldParameter, FlatVector}
import squantlib.model.rates._
import squantlib.model.fx.{FX, FX_novol, FX_flatvol, FX_nosmile, FX_smiled}
import org.jquantlib.currencies.Currency
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod, TimeUnit, Calendar}
import org.jquantlib.instruments.Bond
import org.jquantlib.pricingengines.bond.DiscountingBondEngine
import org.jquantlib.termstructures.YieldTermStructure

/** 
 * Stores rate curve information and initialize discount curves as requested.
 * Require all discount curves to have same value date.
 * 
 * @param Map CurrencyId => DiscountCurve
 */
class CurveFactory(val curves:Map[String, DiscountableCurve], val cdscurves:Map[String, CDSCurve] = null, val paramset:String = null) {

	var valuedate:qlDate = curves.head._2.valuedate
	require(curves.forall(_._2.valuedate == valuedate))
	
	val FX_baseSpread = 0.0
	val FX_baseCurrency = "USD"
	
	/** 
	 * USD
	 */ 
	val pivotCurrency:String = BasisSwapCurve.pivotcurrency.code
	
	/** 
	 * Currencies
	 */
	val currencies:Iterable[Currency] = curves.collect { case (k, v) => v.currency }
	val curveList:Set[String] = curves.keySet

	/** 
	 * Issuers
	 */
	val cdsNames:Set[String] = if (cdscurves == null) null else cdscurves.keySet
	
	/** 
	 * Discounting Curves
	 */
	val discountingCurves:Map[String, RateCurve] = curves.collect{ case (cur:String, curve:RateCurve) => (cur, curve)}
	  
	/**
	 * Stores already calculated discount curves.
	 * Assumption: for each key, value contains discount curve for both discount and pivot currency.
	 */
	var repository:Map[String, scala.collection.mutable.Map[String, DiscountCurve]] = Map.empty
	
	/**
	 * Returns FX spot ccy1 / ccy2
	 * @param currency code, 
	 */
	def fx(ccy1:String, ccy2:String):Double = 
	  try {curves(ccy2).fx / curves(ccy1).fx } catch { case _ => Double.NaN}

	/**
	 * Returns discount curve. Discount currency is flat and same currency with given spread.
	 * @param currency code, spread
	 */
	def getDiscountCurve(ccy:String, spread:Double):Option[DiscountCurve] = 
	  getDiscountCurve(ccy, ccy, spread)
	
	/**
	 * Returns discount curve, flat spread, using specific currency.
	 * @param currency code, discounting currency name, spread
	 */
	def getDiscountCurve(ccy:String, discountccy:String, spread:Double) : Option[DiscountCurve] = 
	  getDiscountCurve(ccy, discountccy, new FlatVector(valuedate, spread), null)

	/**
	 * Returns discount curve using spread of given cds curve.
	 * @param currency code, cds id
	 */
	def getDiscountCurve(ccy:String, cdsid:String) : Option[DiscountCurve] = 
	  if (cdsNames.isEmpty || cdsNames.contains(cdsid)) getDiscountCurve(ccy, cdscurves(cdsid).currency.code, cdscurves(cdsid).rate, cdsid)
	  else None

	/**
	 * Returns discount curve from given CDS curve.
	 * @param currency code, CDS curve
	 */
	def getDiscountCurve(ccy:String, spread:CDSCurve) : Option[DiscountCurve] = 
	  getDiscountCurve(ccy, spread.currency.code, spread.rate, null)
	
	/**
	 * Returns discount curve from full given parameter.
	 */
	private def getDiscountCurve(ccy:String, discountccy:String, spread:YieldParameter, cdsid:String) : Option[DiscountCurve] = 
	  if (contains(ccy, cdsid)) Some(repository(cdsid)(ccy))
	  else {
	    val newcurve = ccy match {
		    case `discountccy` => { curves(ccy).getZC(spread) }
		    					
		    case `pivotCurrency` => { 
			    val zccurve = getDiscountCurve(discountccy, discountccy, spread, cdsid).get
			    curves(ccy).getZC(ratecurve(discountccy), zccurve)
			    }
		      
		    case _ => { 
			    val pivotZC = getDiscountCurve(pivotCurrency, discountccy, spread, cdsid).get
			    curves(ccy).getZC(ratecurve(pivotCurrency), pivotZC)
			    }
    	}
	    
	    if (cdsid != null) {
		    if (!repository.keySet.contains(cdsid)) repository += (cdsid -> scala.collection.mutable.Map(ccy -> newcurve))
		    else repository(cdsid) += (ccy -> newcurve)}
	    
	    Some(newcurve)
	  }
	
	private def ratecurve(c:String):RateCurve = 
	  if (discountingCurves.keySet.contains(c)) discountingCurves(c) 
	  else throw new ClassCastException
	
	/**
	 * Returns zero volatility FX object representing the FX exchange rate between given currencies.
	 * @param currency code
	 */
	def getFX(ccy1:String, ccy2:String) : Option[FX] = 
	  FX_novol(getDiscountCurve(ccy1, FX_baseCurrency, FX_baseSpread).orNull, getDiscountCurve(ccy2, FX_baseCurrency, FX_baseSpread).orNull)
	
	/**
	 * Returns flat volatility FX object representing the FX exchange rate between given currencies.
	 * @param currency code
	 * @param volatility (flat over timeline & strike)
	 */
	def getFX(ccy1:String, ccy2:String, vol:Double) : Option[FX] = 
	  FX_flatvol(getDiscountCurve(ccy1, FX_baseCurrency, FX_baseSpread).orNull, getDiscountCurve(ccy2, FX_baseCurrency, FX_baseSpread).orNull, vol)
	
	/**
	 * Returns non-smiled volatility FX object representing the FX exchange rate between given currencies.
	 * @param currency code
	 * @param volatility as function of time t
	 */
	def getFX(ccy1:String, ccy2:String, vol:Long => Double) : Option[FX] = 
	  FX_nosmile(getDiscountCurve(ccy1, FX_baseCurrency, FX_baseSpread).orNull, getDiscountCurve(ccy2, FX_baseCurrency, FX_baseSpread).orNull, vol)

	/**
	 * Returns smiled volatility FX object representing the FX exchange rate between given currencies.
	 * @param currency code
	 * @param volatility as function of time t and strike k
	 */
	def getFX(ccy1:String, ccy2:String, vol:(Long, Double) => Double) : Option[FX] = 
	  FX_smiled(getDiscountCurve(ccy1, FX_baseCurrency, FX_baseSpread).orNull, getDiscountCurve(ccy2, FX_baseCurrency, FX_baseSpread).orNull, vol)
	
	def getYieldTermStructure(bond:Bond):Option[YieldTermStructure] = 
	  	try { Some(getDiscountCurve(bond.currency.code, bond.creditSpreadID).get.toZCImpliedYieldTermStructure) } 
		catch { case e:Exception => None}
										   
	def getYieldTermStructure(bond:Bond, calendar:Calendar):Option[YieldTermStructure] = 
		try { Some(getDiscountCurve(bond.currency.code, bond.creditSpreadID).get.toZCImpliedYieldTermStructure(calendar))} 
		catch { case e:Exception => None}
	
	def getCustomYieldTermStructure(bond:Bond, calendar:Calendar, newvaluedate:qlDate):Option[YieldTermStructure] = 
		try { 
		  val newcurve = getDiscountCurve(bond.currency.code, bond.creditSpreadID).get
		  newcurve.valuedate = newvaluedate
		  Some(newcurve.toZCImpliedYieldTermStructure(calendar))
		  } 
		catch { case e:Exception => None}
	
	def getDiscountBondEngine(bond:Bond):Option[DiscountingBondEngine] = 
	  	try { Some(getDiscountCurve(bond.currency.code, bond.creditSpreadID).get.toDiscountBondEngine) } 
		catch { case e:Exception => None}
		
	def getDiscountBondEngine(bond:Bond, calendar:Calendar):Option[DiscountingBondEngine] = 
		try { Some(getDiscountCurve(bond.currency.code, bond.creditSpreadID).get.toDiscountBondEngine(calendar)) } 
		catch { case e:Exception => None}
	
	def getCustomDiscountBondEngine(bond:Bond, calendar:Calendar, newvaluedate:qlDate):Option[DiscountingBondEngine] = 
		try { 
		  val newcurve = getDiscountCurve(bond.currency.code, bond.creditSpreadID).get
		  newcurve.valuedate = newvaluedate
		  Some(newcurve.toDiscountBondEngine(calendar)) } 
		catch { case e:Exception => None}
	
	/**
	 * Checks whether the given curve is already calculated and stored in the repository.
	 */
	def contains(ccy:String, cdsid:String) = {
		repository.contains(cdsid) && repository(cdsid).contains(ccy)
	 }
	
	def describe = {
		val eol = sys.props("line.separator")
		val sortedcurves = scala.collection.immutable.TreeMap(curves.toArray:_*)	    
		val sortedcdscurves = scala.collection.immutable.TreeMap(cdscurves.toArray:_*)	    
		"Curves:" + eol + sortedcurves.map(c => c._2.describe + (if (discountingCurves.keySet.contains(c._1)) "(*)" else "") + eol).mkString("") + 
		"(*) Discounting curves" + eol + eol +
		"Credit Spreads:" + eol + sortedcdscurves.map(c => c._1 + "\t" + c._2.rate.valuedate.shortDate + "\t" + c._2.rate.maxdate.shortDate + eol).mkString("")
	}
	
    override def toString():String = "DiscountCurveFactory{" + curves.map(c => c._2).mkString(", ") + "}"
	
//    def this(curves:Map[String, DiscountableCurve]) = this(curves, null, null)
//    def this(curves:Map[String, DiscountableCurve], cdscurves:Map[String, CDSCurve]) = this(curves, cdscurves, null)
}

