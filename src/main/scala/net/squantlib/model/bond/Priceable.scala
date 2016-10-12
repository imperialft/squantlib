package net.squantlib.model.bond

import net.squantlib.util.Date
import net.squantlib.schedule.payoff._
import net.squantlib.util.{SimpleCache, FormulaParser}
import net.squantlib.pricing.model.PricingModel
import net.squantlib.math.solver._
import net.squantlib.model.market.Market
import net.squantlib.model.rates.DiscountCurve
import net.squantlib.model.asset.Underlying
import net.squantlib.util.{UnderlyingParser, UnderlyingParsers}
import scala.collection.mutable.ArrayBuffer
import net.squantlib.util.initializer.Currencies
import scala.language.postfixOps
import net.squantlib.util.DisplayUtils._

/**
 * Bond class with enclosed risk analysis functions.
 */ 

trait Priceable extends ExtendedSchedule with Cloneable {
  
  self : BondModel =>
  
  var _market:Option[Market]
  
  var model:Option[PricingModel]
  
  def initializeModel(reCalibrate:Boolean = false):Unit
  
  def reset(newMarket:Market, setter:(Market, BondModel) => Option[PricingModel])
    
  override def market:Option[Market] = _market
  
  def market_= (newMarket:Market) = {
    val recalib = market.collect{case mkt => mkt.valuedate.eq(newMarket.valuedate) }.getOrElse(true)
    _market = Some(newMarket)
    initializeModel(recalib)
  }
  
  def setMarketNoCalibration(newMarket:Market) = {
    _market = Some(newMarket)
    initializeModel(false)
  }
  
  def getUnderlyings:Map[String, Option[Underlying]] = market match {
    case None => underlyings.map(u => (u, None)) (collection.breakOut)
    case Some(mkt) => underlyings.map(u => (u, Underlying(u, mkt))) (collection.breakOut)
  }
  
  /* 
   * True if necessary to run calibrateModel to get accurate price.
   */
  def calibrateModel = model match {
    case Some(m) => 
      model = Some(m.calibrate)
      cache.clear
    case None => {}
  }
  
  /*
   * Cache to store temporary values (currently used for spot and forward coupons)
   */
  override val cache = new SimpleCache
  val calibrationCache = new SimpleCache
  def getCalibrationCache(k:String):Option[Any] = calibrationCache.getAny(k)  
  
  def clearCache = {
    cache.clear
    calibrationCache.clear
    model.collect{case m => m.modelCache.clear}
  }
  
  
  /*  
   * List of "live" FX underlyings
   */
  def fxList:Set[String] = livePayoffs.payoffs.underlyings.filter(
    c => ((c.size == 6) && (Currencies contains (c take 3)) && (Currencies contains (c takeRight 3))))

  
  /*  
   * Returns discount curve.
   *   @returns discount curve created from either pre-set or specified market
   */
  def discountCurve:Option[DiscountCurve] = market.flatMap(m => m.getDiscountCurve(currency, db.issuerid))
  
  /*  
   * Returns discount curve.
   *   @returns discount curve created from either pre-set or specified market
   */
  def discountFactors:Option[List[(Date, Double)]] = (discountCurve, valueDate) match {
    case (Some(curve), Some(vd)) => Some(schedule.paymentDates.withFilter(_ gt vd).map(d => (d, curve(d))))
    case _ => None
  }

  /*  
   * Returns dirty price of the bond. (ie. including accrued interest)
   */
  def dirtyPrice:Option[Double] = 
    if(!scheduledPayoffs.isPriceable) {errorOutput(id, "invalid payoff or trigger"); None}
    else (earlyTerminationDate, valueDate) match {
      case (Some(td), Some(vd)) if td le vd => standardOutput(id, "terminated on " + td); None
      case _ => (model, discountCurve) match {
        case (Some(m), Some(c)) => m.price(c)
        case (Some(m), None) => errorOutput(id, "missing discount curve"); m.price
        case _ => errorOutput(id, "missing model"); None
  }}
  
  def dirtyPriceWithPaths(nbPath:Int):Option[Double] = 
    if(!scheduledPayoffs.isPriceable) {errorOutput(id, "invalid payoff or trigger"); None}
    else (earlyTerminationDate, valueDate) match {
      case (Some(td), Some(vd)) if td le vd => standardOutput(id, "terminated on " + td); None
      case _ => model match {
        case Some(m) => 
          val originalPaths = m.mcPaths
          m.mcPaths = nbPath
          val result = discountCurve match {
            case Some(c) => m.price(c)
            case None => errorOutput(id, "missing discount curve"); m.price
          }
          m.mcPaths = originalPaths
          result
        case _ => errorOutput(id, "missing model"); None
  }}
  
  /*  
   * Returns JPY dirty price defined as price x FX/FX0, where FX0 = FX as of issue date.
   */
  def dirtyPriceJpy:Option[Double] = (dirtyPrice, fxjpy, db.initialfx) match { 
    case (Some(p), Some(fx), init) if init > 0 => Some(p * fx / init)
    case _ => None
  }
  
  /*  
   * Returns clean price of the bond (ie. Dirty price - accrued coupon)
   */
  def cleanPrice:Option[Double] = (dirtyPrice, accruedAmount) match { 
    case (Some(d), Some(a)) => Some(d - a) 
    case _ => None
  }
  
  /*  
   * Returns JPY clean price defined as price x FX/FX0, where FX0 = FX as of issue date.
   */
  def cleanPriceJpy:Option[Double] = (cleanPrice, fxjpy, db.initialfx) match { 
    case (Some(p), Some(fx), init) if init > 0 => Some(p * fx / init)
    case _ => None
  }
  
  /*  
   * Returns accrued coupon.
   */
  def accruedAmount:Option[Double] = market.flatMap(mkt => 
    if (issueDate ge mkt.valuedate) Some(0.0)
    else if (coupon isEmpty) Some(0.0)
    else livePayoffs.filter{case (d, p, _) => !d.isAbsolute} match {
      case po if po.size == 1 && po.head._1.paymentDate == terminationDate =>
        po.head match {case (dd, pp, _) => Some(Date.daycount(dd.startDate, mkt.valuedate, dd.daycounter) * pp.price(mkt))}
      case po => po.filter{case (dd, pp, _) => (dd.isCurrentPeriod(mkt.valuedate))} match {
        case pos if pos.isEmpty => Some(0.0)
        case pos => Some(pos.map{case (ddd, ppp, _) => (ddd.accrued(mkt.valuedate)) * ppp.price(mkt)}.sum)
    }})
    
  /*  
   * Returns JPY accrued amount defined as accrued x FX/FX0, where FX0 = FX as of issue date.
   */
  def accruedAmountJpy:Option[Double] = (accruedAmount, fxjpy, db.initialfx) match { 
    case (Some(p), Some(fx), init) if init > 0 => Some(p * fx / init)
    case _ => None
  }
  
  /*  
   * Returns dirty price of the bond using the model parameters.
   */
  def modelPrice:Option[Double] = (model, discountCurve) match {
    case (Some(m), Some(c)) => m.modelPrice(c)
    case _ => None
  }
  
  /*  
   * Returns spot FX rate against JPY
   */
  def fxjpy:Option[Double] = market.flatMap (mkt => mkt.fx(currency.code, "JPY"))
  
  def modelPriceJpy:Option[Double] = (modelPrice, fxjpy, db.initialfx) match { 
    case (Some(p), Some(fx), init) if init > 0 => Some(p * fx / init)
    case (Some(p), Some(fx), init) if isIssued == Some(false) => Some(p)
    case _ => None
  }
  
  /*  
   * Returns FX at which JPY dirty bond price becomes 100% at any given date
   */
  def fxFrontier:List[Option[Double]] = nextBermudan match {
    case Some(d) => fxFrontier(1.00, 0.001, 20, d)
    case None => List.fill(underlyings.size)(None)
  }
  
  def fxFrontier(vd:Date):List[Option[Double]] = fxFrontier(1.00, 0.001, 20, vd)

  def fxFrontier(target:Double, 
    accuracy:Double, 
    maxIteration:Int, 
    vd:Date, 
    paths:Int = 0, 
    solver:RangedRootFinder = Bisection, 
    highRange:Double = 10.0, 
    lowRange:Double = 0.01):List[Option[Double]] = (market, valueDate) match {
    
    case (Some(mkt), Some(valuedate)) if dirtyPriceWithPaths(100).isDefined =>
      val bond = dateShifted(valuedate.sub(vd).toInt)
      
      underlyings.map(ul => {
        if (ul.size != 6 || ul.takeRight(3) != "JPY") None
        else {
          val ccy = ul take 3
          
          def priceFromFXmult(y:Double):Double = {
            bond.setMarketNoCalibration(mkt.fxShifted(Map(ccy -> y)))
            if (paths > 0) bond.model.collect{case m => m.mcPaths = paths}
            bond.dirtyPrice.getOrElse(Double.NaN)
          }
          
          val priceformula = (y:Double) => (priceFromFXmult(y) - target)
          val mult = solver.solve(priceformula, lowRange, highRange, accuracy, maxIteration)
          mult.collect{case m => mkt.fx(ccy, "JPY").getOrElse(Double.NaN) / m}
        }})
      
    case _ => List.fill(underlyings.size)(None)
  }
    
  def fxFrontiers:List[List[Option[Double]]] = fxFrontiers(1.00, 0.001, 20)
    
  def fxFrontiers(target:Double, accuracy:Double, maxIteration:Int, paths:Int = 0):List[List[Option[Double]]] = {
    measuredProcess[List[List[Option[Double]]]](db.id, "FX frontiers", false, x => x.map(t => t.map(tt => tt.asDouble).mkString(" ")).mkString(",")) {
      val valuedates = livePayoffs
        .zipWithIndex
        .filter{case (c, _) => c._3.isBermuda && !c._3.isTrigger}
        .map{case (c, index) => (c._1.paymentDate, index)}
        .sortBy{case (date, _) => date}
        .reverse
      
      val tempTrigger = ArrayBuffer(liveTriggers:_*)
      
      valuedates.foreach{case (vd, index) => 
        val tempBond = triggerShifted(tempTrigger.toList)
        tempTrigger(index) = tempBond.fxFrontier(1.00, accuracy, maxIteration, vd, paths)
      }
      tempTrigger.toList
    }
  }
  
  /*  
   * Returns next bermudan callable date.
   */
  def nextBermudan:Option[Date] = {
    val bermdates = liveBermudans.filter{case (d, c) => c}.map{case (d, c) => d.paymentDate}
    if (bermdates.isEmpty) None else Some(bermdates.min)
  }
  
  /*  
   * Returns current coupon rate.
   */
  def currentRate:Option[Double] = 
    if (liveCoupons.isEmpty || market.isEmpty) None
    else liveCoupons.minBy{case (d, _, _) => d.paymentDate} match {case (_, p, _) => p.price(market.get) match {
      case pr if pr.isNaN || pr.isInfinity => None
      case pr => Some(pr)
    }}
  
    
  /*  
   * Returns next coupon payment date
   */
  def nextPayment:Option[(Date, Double)] = 
    if (liveCoupons.isEmpty || market.isEmpty) None
    else liveCoupons.minBy{case (d, _, _) => d.paymentDate} match {case (d, p, _) => (d.dayCount * p.price(market.get)) match {
      case pr if pr.isNaN || pr.isInfinity => None
      case pr => Some(d.paymentDate, pr)
   }}
    
  /*  
   * Returns FX at which JPY dirty bond price becomes 100% (if any)
   */
  def parMtMfx:Option[Double] = if (currency.code == "JPY") None else dirtyPrice.collect{case p => db.initialfx / p }
  
  /*  
   * Returns present value of adding 1 basis point of coupon for the remainder of the bond.
   */
  def bpvalue:Option[Double] = (valueDate, discountCurve) match {
    case (Some(vd), Some(curve)) => Some(livePayoffs.schedule.map{
      case d if d.isAbsolute => 0.0
      case d => d.dayCountAfter(vd) * curve(d.paymentDate)
    }.sum * 0.0001) 
    case _ => None
  }
  
  
  def dateShifted(shift:Int):Priceable
    
  def triggerShifted(trig:List[List[Option[Double]]]):Priceable
  
}

