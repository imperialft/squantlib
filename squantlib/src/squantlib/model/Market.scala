package squantlib.model

import scala.collection.mutable.{HashMap, WeakHashMap}
import scala.collection.JavaConversions
import squantlib.model.yieldparameter.{YieldParameter, FlatVector}
import squantlib.model.rates._
import squantlib.model.fx._
import squantlib.database.schemadefinitions.{CDSParameter, RateFXParameter}
import org.jquantlib.currencies.Currency
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod, TimeUnit, Calendar}
import org.jquantlib.instruments.{Bond => qlBond}
import org.jquantlib.pricingengines.bond.DiscountingBondEngine
import org.jquantlib.termstructures.YieldTermStructure

/** 
 * Stores market information and initialize discount curves as requested.
 * Require all discount curves to have same value date.
 * 
 * @param Map CurrencyId => DiscountCurve
 */
class Market(
    val curves:Map[String, DiscountableCurve], 
    val cdscurves:Map[String, CDSCurve] = Map.empty, 
    val fxparams:Map[String, FXparameter] = Map.empty, 
    val paramset:String = null, 
    val fixings:Map[String, Double]  = Map.empty) {

	/** 
	 * Market value date
	 */ 
	var valuedate:qlDate = curves.head._2.valuedate
	
	require(curves.forall(_._2.valuedate == valuedate))
	
	/** 
	 * Arbitrage-free discount currency for computing FX forward. 
	 */ 
	var FXbaseCurrency = "USD"
	  
	/** 
	 * Arbitrage-free discount spread for computing FX forward. 
	 */ 
	var FXbaseSpread = 0.0
	
	/** 
	 * Pivot currency for basis swap
	 */ 
	val pivotCurrency:String = BasisSwapCurve.pivotcurrency.code
	
	/** 
	 * Registered currencies
	 */
	val currencies:Set[Currency] = curves.collect { case (k, v) => v.currency }.toSet
	
	/** 
	 * Discounting Curves
	 */
	val discountingCurves:Map[String, RateCurve] = curves.collect{ case (cur:String, curve:RateCurve) => (cur, curve)}
	  
	/**
	 * Stores already calculated discount curves.
	 * Assumption: for each key, value contains discount curve for both discount and pivot currency.
	 */
	var repository = new WeakHashMap[String, scala.collection.mutable.Map[String, DiscountCurve]]
	
	/**
	 * Returns FX spot ccy1 / ccy2
	 */
	def fx(ccy1:String, ccy2:String):Option[Double] = 
	  try { 
	    val calc = curves(ccy2).fx / curves(ccy1).fx
	    if (calc.isNaN) None else Some(calc)}
	  catch { case _ => None}

	/**
	 * Returns discount curve with base spread & currency. (3m USDL flat)
	 */
	def getBaseDiscountCurve(ccy:String):Option[DiscountCurve] = getDiscountCurve(ccy, FXbaseCurrency, FXbaseSpread)
	
	def getBaseDiscountCurve(ccy:Currency):Option[DiscountCurve] = getBaseDiscountCurve(ccy.code)
	  
	/**
	 * Returns discount curve. Discounting currency is the same currency with given flat spread.
	 */
	def getDiscountCurve(ccy:String, spread:Double):Option[DiscountCurve] = getDiscountCurve(ccy, ccy, spread)
	
	def getDiscountCurve(ccy:Currency, spread:Double):Option[DiscountCurve] = getDiscountCurve(ccy.code, spread)
	
	/**
	 * Returns discount curve using discounting currency with given flat spread.
	 */
	def getDiscountCurve(ccy:String, discountccy:String, spread:Double):Option[DiscountCurve] = getDiscountCurve(ccy, discountccy, new FlatVector(valuedate, spread), null)
	  
	def getDiscountCurve(ccy:Currency, discountccy:String, spread:Double):Option[DiscountCurve] = getDiscountCurve(ccy.code, discountccy, spread)

	/**
	 * Returns discount curve using spread of given cds curve.
	 */
	def getDiscountCurve(ccy:String, cdsid:String) : Option[DiscountCurve] = 
	  if (cdscurves.contains(cdsid)) getDiscountCurve(ccy, cdscurves(cdsid).currency.code, cdscurves(cdsid).rate, cdsid)
	  else None
	  
	def getDiscountCurve(ccy:Currency, cdsid:String) : Option[DiscountCurve] = getDiscountCurve(ccy.code, cdsid)

	/**
	 * Returns discount curve from given CDS curve.
	 * @param currency code, CDS curve
	 */
	def getDiscountCurve(ccy:String, spread:CDSCurve) : Option[DiscountCurve] = getDiscountCurve(ccy, spread.currency.code, spread.rate, null)
	
	def getDiscountCurve(ccy:Currency, spread:CDSCurve) : Option[DiscountCurve] = getDiscountCurve(ccy.code, spread)
	  
	/**
	 * Returns discount curve from full given parameter.
	 */
	private def getDiscountCurve(ccy:String, discountccy:String, spread:YieldParameter, cdsid:String) : Option[DiscountCurve] = 
	  if (!curves.contains(ccy)) {println(paramset + " : curve " + ccy + " not found"); None}
	  else if (contains(ccy, cdsid)) Some(repository(cdsid)(ccy))
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
		    if (!repository.contains(cdsid)) repository += (cdsid -> scala.collection.mutable.Map(ccy -> newcurve))
		    else repository(cdsid) += (ccy -> newcurve)}
	    
	    Some(newcurve)
	  }
	
	private def ratecurve(c:String):RateCurve = 
	  if (discountingCurves.contains(c)) discountingCurves(c) 
	  else throw new ClassCastException
	
	/**
	 * Returns zero volatility FX object representing the FX exchange rate between given currencies.
	 * @param currency code
	 */
	def getFX(ccyFor:String, ccyDom:String) : Option[FX] = {
	    val curveDom = getBaseDiscountCurve(ccyDom)
	    val curveFor = getBaseDiscountCurve(ccyFor)
	    if ((curveDom isDefined) && (curveFor isDefined)) {
	      if (fxparams.contains(ccyFor + ccyDom)) fxparams(ccyFor + ccyDom).getModel(curveDom.get, curveFor.get)
	      else Some(FXzeroVol(curveDom.get, curveFor.get))
	    }
	    else None
	  }
	
	def getFX(fxpair:String):Option[FX] = {
	  if (fxpair == null || fxpair.size != 6) None
	  else getFX(fxpair.substring(0, 3), fxpair.substring(3, 6))
	}
	
	/**
	 * Returns flat volatility FX object representing the FX exchange rate between given currencies.
	 * @param currency code
	 * @param volatility (flat over timeline & strike)
	 */
	def getFX(ccyFor:String, ccyDom:String, vol:Double) : Option[FX] = {
	    val curveDom = getBaseDiscountCurve(ccyDom)
	    val curveFor = getBaseDiscountCurve(ccyFor)
	    if ((curveDom isDefined) && (curveFor isDefined)) FXflatVol(curveDom.get, curveFor.get, vol) else None
	}
	
	/**
	 * Returns non-smiled volatility FX object representing the FX exchange rate between given currencies.
	 * @param currency code
	 * @param volatility as function of time t
	 */
	def getFX(ccyFor:String, ccyDom:String, vol:Double => Double) : Option[FX] = {
	    val curveDom = getBaseDiscountCurve(ccyDom)
	    val curveFor = getBaseDiscountCurve(ccyFor)
	    if ((curveDom isDefined) && (curveFor isDefined)) FXnoSmile(curveDom.get, curveFor.get, vol) else None
	}

	/**
	 * Returns smiled volatility FX object representing the FX exchange rate between given currencies.
	 * @param currency code
	 * @param volatility as function of time t and strike k
	 */
	def getFX(ccyFor:String, ccyDom:String, vol:(Double, Double) => Double) : Option[FX] = {
	    val curveDom = getBaseDiscountCurve(ccyDom)
	    val curveFor = getBaseDiscountCurve(ccyFor)
	    if ((curveDom isDefined) && (curveFor isDefined)) FXsmiled(curveDom.get, curveFor.get, vol) else None
	}
	
	/**
	 * Returns rate curve for the given currency.
	 * @param currency code
	 * @returns curve if given currency is initialized as rate curve
	 */
	def getRateCurve(ccy:String):Option[RateCurve] = 
	  curves.get(ccy).flatMap(_ match { case c:RateCurve => Some(c); case _ => None})
	
	def getCash(ccy:String, maturity:qlPeriod):Option[Double] = 
	  getRateCurve(ccy).flatMap(c => try {Some(c.cash(maturity))} catch { case _ => None })
	
	def getSwap(ccy:String, maturity:qlPeriod):Option[Double] = 
	  getRateCurve(ccy).flatMap(c => try {Some(c.swap(maturity))} catch { case _ => None })
	
	def getBasis(ccy:String, maturity:qlPeriod):Option[Double] = 
	  getRateCurve(ccy).flatMap(c => try {Some(c.basis(maturity))} catch { case _ => None })
	
	def get3M6M(ccy:String, maturity:qlPeriod):Option[Double] = 
	  getRateCurve(ccy).flatMap(c => try {Some(c.tenorbasis(maturity))} catch { case _ => None })
	  
	/**
	 * Returns FX curve for the given currency.
	 * @param currency code
	 * @returns curve if given currency is initialized as FX curve
	 */
	def getFXCurve(ccy:String):Option[FXCurve] = 
	  curves.get(ccy).flatMap(_ match { case c:FXCurve => Some(c); case _ => None})
	  
	def getSwapPoint(ccy:String, maturity:qlPeriod):Option[Double] = 
	  getFXCurve(ccy).flatMap(c => try {Some(c.swappoint(maturity))} catch { case _ => None })
	
	def getFixings(params:Set[String]):Map[String, Double] = params.map(p => (p, getFixing(p))).collect{case (n, Some(p)) => (n, p)}.toMap
	
	def getFixing(param:String):Option[Double] = 
	  if (param == null) None
	  else param.trim match {
	      case p if fixings contains p => Some(fixings(p))
		  case "CMT10" => fixings.get("JGBY10Y")
		  case p if p.size <= 3 => None
		  case p => (p take 3, p substring 3) match {
		    case (ccy, _) if !isCcy(ccy) => None
		    case (ccy1, ccy2) if isCcy(ccy2) => fx(ccy1, ccy2)
		    case (ccy, mat) if !isNumber(mat dropRight 1) => None
		    case (ccy, mat) if cashPeriods contains (mat takeRight 1) => getCash(ccy, new qlPeriod(mat))
		    case (ccy, mat) if swapPeriods contains (mat takeRight 1) => getSwap(ccy, new qlPeriod(mat))
		    case _ => None
		  }
	}
	
	/**
	 * Returns market after shifting rate curve(s).
	 * Shift for swap-point defined curves are approximate.
	 * @param currency code
	 * @param map with shift on each curve (additive)
	 */
	def rateShifted(currency:String, shiftAmount:Double):Market = rateShifted(Map(currency -> shiftAmount))
	def rateShifted(rateShift:Map[String, Double]):Market = {
	  val basecurve = getBaseDiscountCurve(FXbaseCurrency).get
	  val equivshift:Map[String, (Double, Double) => Double] = rateShift.filter{case (k, v) => curves contains k}
	  			.map { case (k, v) =>
	  			  curves(k) match {
	  			    case curve:RateCurve => (k, (d:Double, r:Double) => r + v)
	  			    case curve:NDSDiscountCurve => (k, (d:Double, r:Double) => r + v)
	  			    case curve:FXCurve => 
	  			      val fx = curve.fx
	  			      val zcf = (t:Double) => basecurve(t)
	  			      val mult = curve.swappoint.multiplier
	  			      val rvector = (d:Double) => getBaseDiscountCurve(k).get.impliedRate(d)
	  			      (k, (d:Double, r:Double) => r + v * mult * d / 365.0 * fx * zcf(d) * math.exp{rvector(d) * d/365.0})
	  			  }}
	  
	  new Market(curves.map{case (c, v) => if (equivshift contains c) (c, v.shiftRate(equivshift(c))) else (c, v)}.toMap, cdscurves, fxparams, paramset, fixings)
	}
	
	/**
	 * Returns market after multiplying fx spot.
	 * @param currency code
	 * @param map with shift on each curve (multiplicative)
	 */
	def fxShifted(currency:String, shiftAmount:Double):Market = fxShifted(Map(currency -> shiftAmount))
	
	def fxShifted(fxShift:Map[String, Double]):Market = 
	  new Market(curves.map{case (c, v) => if (fxShift contains c) (c, v.multFX(fxShift(c))) else (c, v)}.toMap, cdscurves, fxparams, paramset, fixings)

	
	/**
	 * Returns market after shifting fx volatility.
	 * @param currency code
	 * @param map with shift on each curve (additive)
	 */
	def fxVolShifted(fxpair:String, shiftAmount:Double):Market = fxVolShifted(Map(fxpair -> shiftAmount))
	
	def fxVolShifted(fxShift:Map[String, Double]):Market = 
	  new Market(curves, cdscurves, fxparams.map{
	    case (c, v) => if (fxShift contains c) (c, v.addFXVol(fxShift(c))) else (c, v)
	    }.toMap, paramset, fixings)
	
	
	private def isCcy(v:String):Boolean = curves.contains(v)
	
	private val cashPeriods = Set("M", "W", "D")
	
	private val swapPeriods = Set("Y")
	
	private def isNumber(v:String):Boolean = try {v.toInt; true} catch {case _ => false}
	  
	
	/** 
	 * Checks whether the given curve is already calculated and stored in the repository.
	 */
	def contains(ccy:String, cdsid:String) = {
		repository.contains(cdsid) && repository(cdsid).contains(ccy)
	 }
	
	
	def show:Unit = {
		val sortedcurves = scala.collection.immutable.TreeMap(curves.toArray:_*)	    
		val sortedcdscurves = scala.collection.immutable.TreeMap(cdscurves.toArray:_*)	    
		println("Curves:")
		sortedcurves.foreach{case (n, c) => println(c.toString + (if (discountingCurves.contains(n)) "(*)" else ""))}
		println("(*) Discounting curves")
		println(" ")
		println("Credit Spreads:")
		sortedcdscurves.foreach{case (n, c) => println(n + "\t" + c.rate.valuedate.shortDate + "\t" + c.rate.maxdate.shortDate)}
	}
	
	def describe = {
		val eol = sys.props("line.separator")
		val sortedcurves = scala.collection.immutable.TreeMap(curves.toArray:_*)	    
		val sortedcdscurves = scala.collection.immutable.TreeMap(cdscurves.toArray:_*)	    
		"Curves:" + eol + sortedcurves.map(c => c._2.valuedate + (if (discountingCurves.contains(c._1)) "(*)" else "") + eol).mkString("") + 
		"(*) Discounting curves" + eol + eol +
		"Credit Spreads:" + eol + sortedcdscurves.map(c => c._1 + "\t" + c._2.rate.valuedate.shortDate + "\t" + c._2.rate.maxdate.shortDate + eol).mkString("")
	}
	
}


object Market {
  
	def apply(ratefxparams:Set[RateFXParameter], cdsparams:Set[CDSParameter]):Option[Market] = if (ratefxparams.isEmpty) None else apply(ratefxparams, cdsparams, new qlDate(ratefxparams.head.paramdate))
	
	def apply(ratefxparams:Set[RateFXParameter], cdsparams:Set[CDSParameter], valuedate:qlDate):Option[Market] = {
	  
	  val liborCurves:Set[LiborDiscountCurve] = LiborDiscountCurve(ratefxparams, valuedate)
	  val fxCurves:Set[FXDiscountCurve] = FXDiscountCurve(ratefxparams, valuedate)
	  val ndsCurves:Set[NDSDiscountCurve] = liborCurves.find(_.currency.code == "USD") match {
	    case None => Set.empty
	    case Some(curve) => NDSDiscountCurve(ratefxparams, curve.getZC(new FlatVector(curve.valuedate, 0.0)), curve.tenorbasis, valuedate)
	  }
	  val discountcurves:Set[DiscountableCurve] = liborCurves ++ fxCurves ++ ndsCurves
	  if (discountcurves.forall(s => s.currency.code != "USD")) {return None}
	  
	  val cdscurves = CDSCurve(cdsparams, valuedate)
	  val fxparams = FXparameter(ratefxparams)
	  val paramset = ratefxparams.head.paramset
	  val fixingParams = Set("Fixing", "Index", "Equity")
	  val fixingset:Map[String, Double] = ratefxparams.withFilter(p => fixingParams contains p.instrument)
	  	.map(p => (p.asset + (if (p.maturity != null) p.maturity else "").trim, p.value)).toMap
	  
	  if (discountcurves.size == 0 || cdscurves.size == 0) None
	  else Some(new Market(
		    discountcurves.map(c => (c.currency.code, c)).toMap, 
		    cdscurves.map(c => (c.issuerid, c)).toMap, 
		    fxparams,
		    paramset,
		    fixingset))
	}
  
}