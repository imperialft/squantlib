package net.squantlib.model.market

import scala.collection.mutable.{HashMap, WeakHashMap, SynchronizedMap}
import scala.collection.breakOut
import net.squantlib.model.yieldparameter.{YieldParameter, FlatVector}
import net.squantlib.model.rates._
import net.squantlib.model.fx._
import net.squantlib.model.index._
import net.squantlib.model.equity._
import net.squantlib.util.Date
import net.squantlib.util.DisplayUtils._
import org.jquantlib.currencies.Currency
import org.jquantlib.time.{Period => qlPeriod}
import net.squantlib.model.index.IndexInitializer
import net.squantlib.util.{UnderlyingParser, UnderlyingParsers}
import net.squantlib.model.asset.Underlying
import scala.language.postfixOps

/** 
 * Stores market information and initialize discount curves as requested.
 * Require all discount curves to have same value date.
 * 
 * @param Map CurrencyId => DiscountCurve
 */
class Market(
    val paramset:String = null, 
    val curves:Map[String, DiscountableCurve] = Map.empty, 
    val cdscurves:Map[String, CDSCurve] = Map.empty, 
    val fxInitializers:Map[String, FXInitializer] = Map.empty,
    val indexInitializers:Map[String, IndexInitializer] = Map.empty,
    val equityInitializers:Map[String, EquityInitializer] = Map.empty,
    val fixings:Map[String, Double]  = Map.empty) {

  /** 
   * Market value date
   */ 
  var valuedate:Date = curves.head._2.valuedate
  
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
  val currencies:Set[Currency] = curves.collect { case (_, v) => v.currency }.toSet
  
  /** 
   * Discounting Curves
   */
  val discountingCurves:Map[String, RateCurve] = curves.collect{ case (cur:String, curve:RateCurve) => (cur, curve)}
    
  /**
   * Stores already calculated discount curves.
   * Assumption: for each key, value contains discount curve for both discount and pivot currency.
   */
  var repository = new WeakHashMap[String, scala.collection.mutable.Map[String, DiscountCurve]] with SynchronizedMap[String, scala.collection.mutable.Map[String, DiscountCurve]]
  
  /**
   * Returns FX spot ccy1 / ccy2
   */
  def fx(ccy1:String, ccy2:String):Option[Double] = 
    try { 
      val calc = curves(ccy2).fx / curves(ccy1).fx
      if (calc.isNaN || calc.isInfinity) None else Some(calc)}
    catch { case _:Throwable => None}

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
    if (cdscurves contains cdsid) getDiscountCurve(ccy, cdscurves(cdsid).currency.code, cdscurves(cdsid).rate, cdsid)
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
  private def getDiscountCurve(ccy:String, discountccy:String, spread:YieldParameter, cdsid:String) : Option[DiscountCurve] = {
    def errorMsg(msg:String) = errorOutput((if (paramset == null) "" else paramset), msg)
      
    if (!curves.contains(ccy)) {errorOutput(paramset, "curve " + ccy + " not found"); None}
    else if (contains(ccy, cdsid)) Some(repository(cdsid)(ccy))
    else {
      val newcurve:Option[DiscountCurve] = ccy match {
        case `discountccy` => 
          val c = curves(ccy)
          if (c.isPivotDiscountable) Some(c.getZC(spread))
          else {
            errorMsg("not discountable")
            None
          }
        case `pivotCurrency` => getDiscountCurve(discountccy, discountccy, spread, cdsid) match {
            case Some(zccurve) => Some(curves(ccy).getZC(ratecurve(discountccy), zccurve))
            case _ => {
              errorMsg("not discountable")
              None
            }
          }
        case _ => 
          getDiscountCurve(pivotCurrency, discountccy, spread, cdsid) match {
            case Some(pivotZC) if pivotZC != null => 
              val c = curves(ccy)
              if (c.isPivotDiscountable) Some(c.getZC(ratecurve(pivotCurrency), pivotZC))
              else {
                errorMsg("not discountable")
                None
              }
            case _ => 
              errorMsg("not found")
              None
          }
      }
      
      (cdsid, newcurve) match {
        case (cds, Some(c)) if cdsid != null =>
          if (!repository.contains(cds)) repository += (cds -> scala.collection.mutable.Map(ccy -> c))
          else repository(cds) += (ccy -> c)
      	case _ => {}
      }
      
      newcurve
    }
  }
  
  private def ratecurve(c:String):RateCurve = 
    if (discountingCurves.contains(c)) discountingCurves(c) 
    else throw new ClassCastException
    
    
  def getCashUnderlying(ccy:String, tenor:qlPeriod):Option[CashRate] = 
    getDiscountCurve(ccy, ccy, 0.0).collect{case c => CashRate(c, tenor, get3M6MCurve(ccy))}
  
  /**
   * Returns zero volatility FX object representing the FX exchange rate between given currencies.
   * @param currency code
   */
  def getFX(ccyFor:String, ccyDom:String) : Option[FX] = {
    val curveDom = getBaseDiscountCurve(ccyDom)
    val curveFor = getBaseDiscountCurve(ccyFor)
    if ((curveDom isDefined) && (curveFor isDefined)) {
      if (fxInitializers.contains(ccyFor + ccyDom)) fxInitializers(ccyFor + ccyDom).getModel(curveDom.get, curveFor.get)
      else if (fxInitializers.contains(ccyDom + ccyFor)) fxInitializers(ccyDom + ccyFor).getNoSmileModel(curveDom.get, curveFor.get)
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
  
  def getIndex(name:String) : Option[Index] = {
    indexInitializers.get(name).flatMap{case initializer => initializer.getModel(this)}
  }
  
  def getEquity(name:String) : Option[Equity] = {
    equityInitializers.get(name).flatMap{case initializer => initializer.getModel(this)}
  }
  
  /**
   * Returns rate curve for the given currency.
   * @param currency code 
   * @returns curve if given currency is initialized as rate curve
   */
  def getRateCurve(ccy:String):Option[RateCurve] = 
    curves.get(ccy).flatMap(_ match { case c:RateCurve => Some(c); case _ => None})
  
  def getCash(ccy:String, maturity:qlPeriod):Option[Double] = 
    getRateCurve(ccy).flatMap(c => try {Some(c.cash(maturity))} catch { case _:Throwable => None })
  
  def getSwap(ccy:String, maturity:qlPeriod):Option[Double] = 
    getRateCurve(ccy).flatMap(c => try {Some(c.swap(maturity))} catch { case _:Throwable => None })
  
  def getBasis(ccy:String, maturity:qlPeriod):Option[Double] = 
    getRateCurve(ccy).flatMap(c => try {Some(c.basis(maturity))} catch { case _:Throwable => None })
  
  def get3M6M(ccy:String, maturity:qlPeriod):Option[Double] = 
    getRateCurve(ccy).flatMap(c => try {Some(c.tenorbasis(maturity))} catch { case _:Throwable => None })
    
  def get3M6MCurve(ccy:String):Option[TenorBasisSwapCurve] = getRateCurve(ccy) match {
    case Some(c) if c.tenorbasis != null => Some(c.tenorbasis)
    case _ => None
  }
    
  /**
   * Returns FX curve for the given currency.
   * @param currency code
   * @returns curve if given currency is initialized as FX curve
   */
  def getFXCurve(ccy:String):Option[FXCurve] = 
    curves.get(ccy).flatMap(_ match { case c:FXCurve => Some(c); case _ => None})
    
  def getSwapPoint(ccy:String, maturity:qlPeriod):Option[Double] = 
    getFXCurve(ccy).flatMap(c => try {Some(c.swappoint(maturity))} catch { case _:Throwable => None })
  
  def getFixings(ids:Set[String]):Map[String, Double] = ids.map(p => (p, getFixing(p))).collect{case (n, Some(p)) => (n, p)} (breakOut)

  def getFixings(ids:List[String]):List[Double] = ids.map(p => getFixing(p).getOrElse(Double.NaN))

  
  def getFixing(id:String):Option[Double] = id match {
    case null => None
    case p if fixings contains p.trim => Some(fixings(p.trim))
    case p => UnderlyingParsers.get(p).flatMap{case p => p.getSpot(this)}
  }
  
  /**
   * Returns market after shifting rate curve(s).
   * Shift for swap-point defined curves are approximate.
   * @param currency code
   * @param map with shift on each curve (additive)
   */
  def rateShifted(id:String, shiftAmount:Double):Market = rateShifted(Map(id -> shiftAmount))
  def rateShifted(shift:Map[String, Double]):Market = {
    val basecurve = getBaseDiscountCurve(FXbaseCurrency).get
    val equivshift:Map[String, (Double, Double) => Double] = shift.filter{case (k, v) => curves contains k}
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
    
    val newcurve:Map[String, DiscountableCurve] = curves.map{case (c, v) => 
      if (equivshift contains c) (c, v.shiftRate(equivshift(c))) 
      else (c, v)} 
      
    new Market(paramset, newcurve, cdscurves, fxInitializers, indexInitializers, equityInitializers, fixings)
  }
  
  /**
   * Returns market after multiplying fx spot.
   * @param currency code
   * @param map with shift on each curve (multiplicative)
   */
  def fxShifted(id:String, shift:Double):Market = fxShifted(Map(id -> shift))
  
  def fxShifted(shift:Map[String, Double]):Market = {
    val newcurve:Map[String, DiscountableCurve] = curves.map{case (c, v) => 
      if (shift contains c) (c, v.multFX(shift(c))) else (c, v)}
      
    new Market(paramset, newcurve, cdscurves, fxInitializers, indexInitializers, equityInitializers, fixings)
  }

  
  /**
   * Returns market after shifting fx volatility.
   * @param currency code
   * @param map with shift on each curve (additive)
   */
  def fxVolShifted(id:String, shift:Double):Market = fxVolShifted(Map(id -> shift))
  
  def fxVolShifted(shift:Map[String, Double]):Market = 
    new Market(
        paramset, 
        curves, 
        cdscurves, 
        fxInitializers.map{case (c, v) => 
          if (shift.contains(c)) (c, v.addFXVol(shift(c)))
          else if (shift.contains(c.takeRight(3) + c.take(3))) (c, v.addFXVol(shift(c.takeRight(3) + c.take(3))))
          else (c, v)}, 
        indexInitializers, 
        equityInitializers, 
        fixings)

  
  /**
   * Returns market after multiplying equity spot.
   * @param equity code
   * @param map with shift on each curve (multiplicative)
   */
  def equityShifted(id:String, shift:Double):Market = equityShifted(Map(id -> shift))
  
  def equityShifted(shift:Map[String, Double]):Market = {
    val newinitializer:Map[String, EquityInitializer] = equityInitializers.map{case (c, v) => 
      if (shift contains c) (c, v.mult(shift(c))) else (c, v)}
      
    new Market(paramset, curves, cdscurves, fxInitializers, indexInitializers, newinitializer, fixings)
  }
  
  /**
   * Returns market after adding equity volatility.
   * @param equity code
   * @param map with shift on each curve (multiplicative)
   */
  def equityVolShifted(id:String, shift:Double):Market = equityVolShifted(Map(id -> shift))
  
  def equityVolShifted(shift:Map[String, Double]):Market = {
    val newinitializer:Map[String, EquityInitializer] = equityInitializers.map{case (c, v) => 
      if (shift contains c) (c, v.addVol(shift(c))) else (c, v)}
      
    new Market(paramset, curves, cdscurves, fxInitializers, indexInitializers, newinitializer, fixings)
  }
  
  /**
   * Returns market after adding equity volatility.
   * @param equity code
   * @param map with shift on each curve (multiplicative)
   */
  def equityDividendShifted(id:String, shift:Double):Market = equityDividendShifted(Map(id -> shift))
  
  def equityDividendShifted(shift:Map[String, Double]):Market = {
    val newinitializer:Map[String, EquityInitializer] = equityInitializers.map{case (c, v) => 
      if (shift contains c) (c, v.addDividend(shift(c))) else (c, v)}
      
    new Market(paramset, curves, cdscurves, fxInitializers, indexInitializers, newinitializer, fixings)
  }
  
  /**
   * Returns market after multiplying equity spot.
   * @param equity code
   * @param map with shift on each curve (multiplicative)
   */
  def indexShifted(id:String, shift:Double):Market = indexShifted(Map(id -> shift))
  
  def indexShifted(shift:Map[String, Double]):Market = {
    val newInitializers:Map[String, IndexInitializer] = indexInitializers.map{case (c, v) => 
      if (shift contains c) (c, v.mult(shift(c))) else (c, v)}
      
    new Market(paramset, curves, cdscurves, fxInitializers, newInitializers, equityInitializers, fixings)
  }
  
  /**
   * Returns market after adding equity volatility.
   * @param equity code
   * @param map with shift on each curve (multiplicative)
   */
  def indexVolShifted(id:String, shift:Double):Market = indexVolShifted(Map(id -> shift))
  
  def indexVolShifted(shift:Map[String, Double]):Market = {
    val newInitializers:Map[String, IndexInitializer] = indexInitializers.map{case (c, v) => 
      if (shift contains c) (c, v.addVol(shift(c))) else (c, v)}
      
    new Market(paramset, curves, cdscurves, fxInitializers, newInitializers, equityInitializers, fixings)
  }
  
  def getSpot(id:String):Option[Double] = UnderlyingParsers.get(id).flatMap{case p => p.getSpot(this)}
  
  def getUnderlying(id:String):Option[Underlying] = UnderlyingParsers.getUnderlying(id, this)
  
  def underlyingShifted(id:String, shift:Double):Option[Market] = UnderlyingParsers.get(id).flatMap{case p => p.getShiftedMarket(shift, this)}
  
  import scala.annotation.tailrec
  
  def underlyingShifted(shift:Map[String, Double]):Option[Market] = {
    @tailrec def shifted(shifts:List[(String, Double)], mkt:Market):Option[Market] = shifts match {
      case Nil => Some(mkt)
      case (k, v)::t => mkt.underlyingShifted(k, v) match {
        case Some(m) => shifted(t, m)
        case None => None
      }
    }
    shifted(shift.toList, this)
  }
  
  def underlyingVolShifted(id:String, shift:Double):Option[Market] = UnderlyingParsers.get(id).flatMap{case p => p.getShiftedVolMarket(shift, this)}
  
  /** 
   * Checks whether the given curve is already calculated and stored in the repository.
   */
  def contains(ccy:String, cdsid:String) = repository.contains(cdsid) && repository(cdsid).contains(ccy)
  
  
  def show:Unit = {
    standardOutput("Curves")
    standardOutput(curves.keySet.toList.sorted.mkString(" "))
    standardOutput("Credit Spreads:")
    standardOutput(cdscurves.keySet.toList.sorted.mkString(" "))
    standardOutput("Indices:")
    standardOutput(indexInitializers.keySet.toList.sorted.mkString(" "))
    standardOutput("Equities:")
    standardOutput(equityInitializers.keySet.toList.sorted.mkString(" "))
    standardOutput("FX:")
    standardOutput(fxInitializers.keySet.toList.sorted.mkString(" "))
    standardOutput("Fixings:")
    standardOutput(fixings.keySet.toList.sorted.mkString(" "))
    
  }
  
  def describe = {
    val eol = sys.props("line.separator")
    val sortedcurves = scala.collection.immutable.TreeMap(curves.toArray:_*)      
    val sortedcdscurves = scala.collection.immutable.TreeMap(cdscurves.toArray:_*)      
    "Curves:" + eol + sortedcurves.map(c => c._2.valuedate + (if (discountingCurves.contains(c._1)) "(*)" else "") + eol).mkString("") + 
    "(*) Discounting curves" + eol + eol +
    "Credit Spreads:" + eol + sortedcdscurves.map(c => c._1 + "\t" + c._2.rate.valuedate.toString + "\t" + c._2.rate.maxdate.toString + eol).mkString("")
  }
	
}


