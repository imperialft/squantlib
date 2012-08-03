package squantlib.model.discountcurve

import scala.collection.mutable.HashMap
import scala.collection.JavaConversions
import squantlib.parameter.yieldparameter.{YieldParameter, FlatVector}
import org.jquantlib.currencies.Currency
import org.jquantlib.time.{Date => JDate, Period => JPeriod, TimeUnit, Calendar}
import org.jquantlib.instruments.Bond
import org.jquantlib.pricingengines.bond.DiscountingBondEngine
import org.jquantlib.termstructures.YieldTermStructure


/** 
 * Stores rate curve information and initialize discount curves as requested.
 * Require all discount curves to have same value date.
 * 
 * @param Map CurrencyId => DiscountCurve
 */
class DiscountCurveFactory(val curves:Map[String, DiscountableCurve], val cdscurves:Map[String, CDSCurve], val paramset:String) {

	val valuedate = curves.head._2.valuedate
	require(curves.forall(c => c._2.valuedate == valuedate))
	
	/** 
	 * USD
	 */
	val pivotcurrency = BasisSwapCurve.pivotcurrency.code
	
	/** 
	 * Currencies
	 */
	val currencies = curves.map { case (k, v) => v.currency }

	/** 
	 * Issuers
	 */
	val cdsnames = if (cdscurves == null) null else cdscurves.keySet
	
	/** 
	 * Discounting Curves
	 */
	val discountingcurves = { curves.map{ case (cur, curve) => (cur, curve match { case r:RateCurve => r; case _ => null})}}.filter{case(k, c) => c != null}
	
	/**
	 * Stores already calculated discount curves.
	 * Assumption: for each key, value contains discount curve for both discount and pivot currency.
	 */
	var repository:Map[String, scala.collection.mutable.Map[String, DiscountCurve]] = Map.empty
	
	/**
	 * Returns FX spot ccy1 / ccy2
	 * @param currency code, 
	 */
	def fx(ccy1:String, ccy2:String):Double = curves(ccy2).fx / curves(ccy1).fx
	

	/**
	 * Returns discount curve. Discount currency is flat and same currency with given spread.
	 * @param currency code, spread
	 */
	def getdiscountcurve(ccy:String, spread:Double):DiscountCurve = getdiscountcurve(ccy, ccy, spread)
	
	/**
	 * Returns discount curve, flat spread, using specific currency.
	 * @param currency code, discounting currency name, spread
	 */
	def getdiscountcurve(ccy:String, discountccy:String, spread:Double) : DiscountCurve = getdiscountcurve(ccy, discountccy, new FlatVector(valuedate, spread), null)

	/**
	 * Returns discount curve using spread of given cds curve.
	 * @param currency code, cds id
	 */
	def getdiscountcurve(ccy:String, cdsid:String) : DiscountCurve = if (cdsnames.isEmpty || cdsnames.contains(cdsid)) getdiscountcurve(ccy, cdscurves(cdsid).currency.code, cdscurves(cdsid).rate, cdsid) else null

	/**
	 * Returns discount curve from given CDS curve.
	 * @param currency code, CDS curve
	 */
	def getdiscountcurve(ccy:String, spread:CDSCurve) : DiscountCurve = getdiscountcurve(ccy, spread.currency.code, spread.rate, null)
	
	/**
	 * Returns discount curve from full given parameter.
	 */
	private def getdiscountcurve(ccy:String, discountccy:String, spread:YieldParameter, cdsid:String) : DiscountCurve = 
	  if (contains(ccy, cdsid)) repository(cdsid)(ccy)
	  else {
	    val newcurve = ccy match {
		    case `discountccy` => { curves(ccy).getZC(spread) }
		    					
		    case `pivotcurrency` => { 
			    val zccurve = getdiscountcurve(discountccy, discountccy, spread, cdsid)
			    curves(ccy).getZC(ratecurve(discountccy), zccurve)
			    }
		      
		    case _ => { 
			    val pivotZC = getdiscountcurve(pivotcurrency, discountccy, spread, cdsid)
			    curves(ccy).getZC(ratecurve(pivotcurrency), pivotZC)
			    }
    	}
	    
	    if (cdsid != null) {
		    if (!repository.keySet.contains(cdsid)) repository += (cdsid -> scala.collection.mutable.Map(ccy -> newcurve))
		    else repository(cdsid) += (ccy -> newcurve)}
	    newcurve
	  }
	
	private def ratecurve(c:String):RateCurve = if (discountingcurves.keySet.contains(c)) discountingcurves(c) else throw new ClassCastException
	
	def getyieldtermstructure(bond:Bond):YieldTermStructure = 
	  	try { getdiscountcurve(bond.currency.code, bond.creditSpreadID).toZCImpliedYieldTermStructure } 
//		catch { case e:Exception => {println("Could not initialise yield termstructure " + bond.bondid + " " + valuedate.shortDate); null}}
		catch { case e:Exception => {null}}
										   
	def getyieldtermstructure(bond:Bond, calendar:Calendar):YieldTermStructure = 
		try { getdiscountcurve(bond.currency.code, bond.creditSpreadID).toZCImpliedYieldTermStructure(calendar)} 
//		catch { case e:Exception => {println("Could not initialise yield termstructure " + bond.bondid + " " + valuedate.shortDate); null}}
		catch { case e:Exception => {null}}
	
	def getdiscountbondengine(bond:Bond):DiscountingBondEngine = 
	  	try { getdiscountcurve(bond.currency.code, bond.creditSpreadID).toDiscountBondEngine } 
//		catch { case e:Exception => {println("Could not initialise bond engine " + bond.bondid + " " + valuedate.shortDate); null}}
		catch { case e:Exception => {null}}
										   
	def getdiscountbondengine(bond:Bond, calendar:Calendar):DiscountingBondEngine = 
		try { getdiscountcurve(bond.currency.code, bond.creditSpreadID).toDiscountBondEngine(calendar) } 
//		catch { case e:Exception => {println("Could not initialise bond engine " + bond.bondid + " " + valuedate.shortDate); null}}
		catch { case e:Exception => {null}}
	
	/**
	 * Checks whether the given curve is already calculated and stored in the repository.
	 */
	def contains(ccy:String, cdsid:String) = {
		repository.keySet.contains(cdsid) && repository(cdsid).keySet.contains(ccy)
	 }
	
	def describe = {
		val eol = sys.props("line.separator")
		val sortedcurves = scala.collection.immutable.TreeMap(curves.toArray:_*)	    
		val sortedcdscurves = scala.collection.immutable.TreeMap(cdscurves.toArray:_*)	    
		"Curves:" + eol + sortedcurves.map(c => c._2.describe + (if (discountingcurves.keySet.contains(c._1)) "(*)" else "") + eol).mkString("") + 
		"(*) Discounting curves" + eol + eol +
		"Credit Spreads:" + eol + sortedcdscurves.map(c => c._1 + "\t" + c._2.rate.valuedate.shortDate + "\t" + c._2.rate.maxdate.shortDate + eol).mkString("")
	}
	
    override def toString():String = "DiscountCurveFactory{" + curves.map(c => c._2).mkString(", ") + "}"
	
    def this(curves:Map[String, DiscountableCurve]) = this(curves, null, null)
    def this(curves:Map[String, DiscountableCurve], cdscurves:Map[String, CDSCurve]) = this(curves, cdscurves, null)
}

