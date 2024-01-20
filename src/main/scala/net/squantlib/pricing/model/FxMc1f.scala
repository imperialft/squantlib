package net.squantlib.pricing.model

import net.squantlib.model.market.Market
import net.squantlib.schedule.payoff.{Payoffs, Payoff}
import net.squantlib.schedule.{ScheduledPayoffs, Schedule, CalculationPeriod}
import net.squantlib.pricing.mcengine._
import net.squantlib.model.fx.FX
import net.squantlib.schedule.call.Callability
import net.squantlib.model.bond.PriceableBond
import net.squantlib.util.JsonUtils._
import net.squantlib.model.rates.DiscountCurve
import net.squantlib.util.{Date, UnderlyingFixing}
import com.fasterxml.jackson.databind.JsonNode
import scala.collection.mutable.{SynchronizedMap, WeakHashMap}
import scala.annotation.tailrec
import net.squantlib.util.DisplayUtils._

case class FxMc1f(
  valuedate:Date,
  mcengine:Montecarlo1f,
  scheduledPayoffs:ScheduledPayoffs,
  fx:FX,
  defaultPaths:Int,
  trigger:List[Option[Double]],
  frontierFunction:() => List[Option[Double]],
  parameterRepository:Any => Unit,
  bondid:String
) extends PricingModel {

  assert(trigger.size == scheduledPayoffs.calls.size, s"Number of calls(${scheduledPayoffs.calls.size}), trigger(${trigger.size})")

//  println("Initialize FXMC1f")

  mcPaths = defaultPaths
//  val bonusAmounts = scheduledPayoffs.bonusAmounts
  val triggerUps = scheduledPayoffs.triggerUps
  val forwardStrikes = scheduledPayoffs.forwardStrikes
  // val underlying:String = scheduledPayoffs.underlyings.headOption.getOrElse("")
  val underlying:String = scheduledPayoffs.underlyings.headOption.getOrElse(fx.id)

//  println(redemamt.toString)


  //  def price(fixings:List[Double], trigger:List[Option[Double]])(implicit d:DummyImplicit):List[Double] = singleUnderlying match {
  //    case Some(ul) => price(fixings.map(f => Map(ul -> f)), trigger.map(t => t.collect{case tr => Map(ul -> tr)}))
  //    case _ => List.fill(payoffs.size)(Double.NaN)
  //  }
  ////  payoffs.price(priceMapper(fixings), trigger, triggerUps, bonusRates, forwardStrikeSingleUnderlying, calls.targetRedemptions, schedule.dayCounts, None)
  //
  //  def price(fixings:List[Double], trigger:List[Option[Double]], trigAmount:List[Double])(implicit d:DummyImplicit):List[Double] = singleUnderlying match {
  //    case Some(ul) => price(fixings.map(f => Map(ul -> f)), trigger.map(t => t.collect{case tr => Map(ul -> tr)}), trigAmount)
  //    case _ => List.fill(payoffs.size)(Double.NaN)
  //  }
  ////  payoffs.price(priceMapper(fixings), trigger, triggerUps, amountToRate(trigAmount), forwardStrikeSingleUnderlying, calls.targetRedemptions, schedule.dayCounts, None)
  lazy val shiftedCalls:List[Callability] = (scheduledPayoffs.calls, trigger).zipped.map{case (c, t) =>
    c.triggerShifted(t.collect{case tt => Map(underlying -> tt)}.getOrElse(Map.empty))
  }.toList

  val callShiftedScheduledPayoffs = scheduledPayoffs.callShifted(shiftedCalls)

  override def modelPaths(paths:Int):List[List[Double]] = modelPaths(paths, (p:List[Double]) => callShiftedScheduledPayoffs.price(fx.id, p))

  def modelPaths(paths:Int, pricing:List[Double] => List[Double]):List[List[Double]] = {
    val mcYears = scheduledPayoffs.eventDateYears(valuedate)
    val (mcdates, mcpaths) = mcengine.generatePaths(mcYears, paths, pricing)
    if (mcdates.sameElements(mcYears)) mcpaths
    else { errorOutput(bondid, "invalid mc dates"); List.empty}
  }

  def mcPriceFullPath(paths:Int):List[Double] = {
    try {
      val mpaths = modelPaths(paths)
      if (mpaths.isEmpty) scheduledPayoffs.price
      else concatList(mpaths).map(_ / paths.toDouble)
    }
    catch {case e:Throwable => errorOutput(bondid, s"MC calculation error : vd ${fx.valuedate} " + e.getStackTrace.mkString(sys.props("line.separator"))); List.empty}
  }

  def mcPrice(paths:Int):List[Double] = {
    try {
      val mcYears = callShiftedScheduledPayoffs.eventDateYears(valuedate) // scheduledPayoffs.eventDateYears(valuedate)
      mcengine.generatePrice(mcYears, paths, (p:List[Double]) => callShiftedScheduledPayoffs.price(fx.id, p))

      //      if (mcdates.sameElements(mcYears)) mcpaths
      //      else {
      //        errorOutput(bondid, "invalid mc dates")
      //        List.empty
      //      }
    }
    catch {case e:Throwable =>
      errorOutput(bondid, s"MC calculation error : vd ${fx.valuedate} ${sys.props("line.separator")} ${e.toString()} ${sys.props("line.separator")} ${e.getStackTrace.mkString(sys.props("line.separator"))}")
      List.empty
    }
  }

  override def modelForward(paths:Int):List[Double] = concatList(modelPaths(paths)).map(_ / paths)

  override def calculatePrice:List[Double] = calculatePrice(mcPaths)

  def calculatePrice(paths:Int):List[Double] = getOrUpdateCache("PRICE"+paths, mcPrice(paths))

  override def triggerProbabilities:Map[Date, Double] = triggerProbabilities(mcPaths)

  def triggerProbabilities(paths:Int):Map[Date, Double] = getOrUpdateCache("TriggerProb"+paths, {
    val maxdate = scheduledPayoffs.schedule.paymentDates.max
    val prices = FxMc1f(valuedate, mcengine, scheduledPayoffs.trigCheckPayoff, fx, defaultPaths, trigger, frontierFunction, parameterRepository, bondid).mcPrice(paths)
    (scheduledPayoffs, prices).zipped
      .map{case ((cp, _, _), price) => (cp.callValueDate, price * cp.dayCount)}
			.groupBy(_._1).map{case (k, vs) => (k, vs.map(_._2).sum)}
  })


  override def calibrate:FxMc1f = {
    val frontier = frontierFunction()
    parameterRepository(frontier)
    modelOutput("exercise_frontier", frontier.map(_.collect{case v => (v * 10000.0).round / 10000.0}.getOrElse(0)))
    val newmodel = FxMc1f(valuedate, mcengine, scheduledPayoffs, fx, mcPaths, frontier, frontierFunction, parameterRepository, bondid)
    newmodel.modelOutput = modelOutput
    newmodel
  }

  def binaryPathMtM(range:Double, discounts:List[Double]):List[Double] => List[Double] = (underlyingPrices:List[Double]) => {
    val prices = (scheduledPayoffs.price(fx.id, underlyingPrices), scheduledPayoffs.schedule.dayCounts, discounts).zipped.map{case (p, dc, zc) => p * dc * zc}

    @tailrec def forwardSum(input:List[Double], result:List[Double]):List[Double]= input match {
      case Nil => result
      case h::t => forwardSum(t, (h + result.headOption.getOrElse(0.0)) :: result)
    }

    val underlyingFixings = scheduledPayoffs.fixingPrices(fx.id, underlyingPrices)
    val remainingMtM = forwardSum(prices.tail.reverse, List(0.0)).zip(discounts).map{case (p, zc) => p / zc}
    //skip the "current" coupon
    (remainingMtM, underlyingFixings, scheduledPayoffs.calls).zipped.map{case (p, ul, c) =>
      (c.triggers.getDouble.get(fx.id), ul.headOption) match{
        case (Some(t), Some(ull)) if ull >= t * (1.0 - range) && ull < t => p
        case _ => 0.0
      }
    }
  }

  override def binarySize(paths:Int, range:Double, curve:DiscountCurve):Map[Date, Double] = getOrUpdateCache("BinarySize"+paths+range, {
    val discounts = scheduledPayoffs.schedule.paymentDates.map(d => curve(d))
    val data = modelPaths(paths, binaryPathMtM(range, discounts))

    data.transpose
      .map(binaries => binaries.sum / binaries.filter(_ != 0.0).size)
			.zip(scheduledPayoffs)
			.map{case (b, (d, _, p)) => (d.callValueDate, p.fixedRedemptionAmountAtTrigger - b)}.toMap

      // .zip(scheduledPayoffs.calls)
      // .map{case (b, p) => p.fixedRedemptionAmountAtTrigger - b}
  })

  override val priceType = "MODEL"

  override val mcEngine = Some(mcengine)

}


object FxMc1f {

  var defaultPaths = 100000
  var frontierPaths = 15000

  def apply(market:Market, bond:PriceableBond, mcengine:FX => Option[Montecarlo1f]):Option[FxMc1f] = apply(market, bond, mcengine, defaultPaths, frontierPaths)

  def apply(market:Market, bond:PriceableBond, mcengine:FX => Option[Montecarlo1f], triggers:List[Option[Double]]):Option[FxMc1f] = apply(market, bond, mcengine, defaultPaths, frontierPaths, triggers)

  def cachedFrontier(bond:PriceableBond):List[Option[Double]] = bond.getCalibrationCache("FXFrontier") match {
    case Some(t:List[Any]) =>
      t.map{
        case Some(v:Double) => Some(v)
        case _ => None
      }.toList
    case _ => List.empty
  }

  def apply(
    market:Market,
    bond:PriceableBond,
    mcengine:FX => Option[Montecarlo1f],
    paths:Int,
    frontierPths:Int
  ):Option[FxMc1f] = {
    //    val frontierTrig = bond.getCalibrationCache("FXMontecarlo1f") match {
    //      case Some(t:List[Any]) =>
    //        t.map{
    //          case Some(v:Double) => Some(v)
    //          case _ => None
    //        }.toList.takeRight(bond.livePayoffCount(market.valuedate))
    //
    //      case _ =>
    //        bond.liveTriggers(market.valuedate).map(t => if (t.isEmpty) None else t.head)
    //    }

    val cachedTrig = cachedFrontier(bond)
    val frontierTrig:List[Option[Double]] = {
      if (cachedTrig.isEmpty) bond.liveTriggers(market.valuedate).map(t => if (t.isEmpty) None else t.head.collect{case v => v.toDouble})
      else cachedTrig.takeRight(bond.livePayoffCount(market.valuedate))
    }

    //    println("frontierTrig")
    //    println(frontierTrig.size)
    //    println(frontierTrig.mkString(", "))

    apply(market, bond, mcengine, paths, frontierPths, frontierTrig)
  }

  def apply(
    market:Market,
    bond:PriceableBond,
    mcengine:FX => Option[Montecarlo1f],
    paths:Int,
    frontierPths:Int,
    triggers:List[Option[Double]]
  ):Option[FxMc1f] = {

    val valuedate = market.valuedate

    val scheduledPayoffs = bond.livePayoffs(valuedate)

    if (scheduledPayoffs.underlyings.size != 1) {
      errorOutput(bond.id, "unsupported variable size for FXMC1 model " + scheduledPayoffs.underlyings.size)
      return None}

    val variable = scheduledPayoffs.underlyings.head

    val fx = market.getFX(variable).orNull

    if (fx == null) {
      errorOutput(bond.id, "invalid fx underlying for FXMC1 model - " + variable + " in market " + market.paramset)
      return None}

    if (fx.currencyDom != bond.currency) {
      errorOutput(bond.id, "quanto model not supported by FXMC1 model - " + variable)
      return None}

    val mcmodel = mcengine(fx).orNull

    if (mcmodel == null) {
      errorOutput(bond.id, "model name not found or model calibration error - FXMc1f")
      return None}

    Some(FxMc1f(valuedate, mcmodel, scheduledPayoffs, fx, paths, triggers, frontierFunction(bond, frontierPths), paramRepository(bond), bond.id))
  }

  def frontierFunction(bond:PriceableBond, frontierPths:Int) = () => bond.fxFrontiers(1.00, 0.003, 20, frontierPths).map(t => if (t.isEmpty) None else t.head)

  def paramRepository(bond:PriceableBond):Any => Unit = (obj:Any) => {
    bond.calibrationCache.update("FXFrontier", obj)
    obj match {
      case fs:List[d] =>
        (bond.livePayoffs, fs).zipped.map{
          case ((_, _, c), Some(f:Double)) => c.simulatedFrontier = UnderlyingFixing(Map(bond.underlyings.head -> f))(bond.fixingInformation)
          case _ => {}}
      case _ => {}
    }
  }

}



object FxQtoMc1f {

  var defaultPaths = 300000
  var frontierPaths = 15000

  def apply(
    market:Market,
    bond:PriceableBond,
    mcengine:(FX, FX) => Option[Montecarlo1f]
  ):Option[FxMc1f] = apply(market, bond, mcengine, defaultPaths, frontierPaths)

  def apply(
    market:Market,
    bond:PriceableBond,
    mcengine:(FX, FX) => Option[Montecarlo1f],
    triggers:List[Option[Double]]
  ):Option[FxMc1f] = apply(market, bond, mcengine, defaultPaths, frontierPaths, triggers)

  def apply(
    market:Market,
    bond:PriceableBond,
    mcengine:(FX, FX) => Option[Montecarlo1f],
    paths:Int,
    frontierPths:Int
  ):Option[FxMc1f] = {

    val trig:List[Option[Double]] = bond.getCalibrationCache("FXFrontier") match {
      case Some(t:List[Any]) => t.map{
        case Some(v:Double) => Some(v)
        case Some(v:BigDecimal) => Some(v.toDouble)
        case _ => None
      }.toList

      case _ => bond.liveTriggers(market.valuedate).map(t => if (t.isEmpty) None else t.head.collect{case v => v.toDouble})
    }

    apply(market, bond, mcengine, paths, frontierPths, trig)
  }

  def apply(
    market:Market,
    bond:PriceableBond,
    mcengine:(FX, FX) => Option[Montecarlo1f],
    paths:Int,
    frontierPths:Int,
    triggers:List[Option[Double]]
  ):Option[FxMc1f] = {

    val valuedate = market.valuedate

    val scheduledPayoffs = bond.livePayoffs(valuedate)

    if (scheduledPayoffs.underlyings.size != 1) {
      errorOutput(bond.id, "unsupported variable size for FXMC1 model " + scheduledPayoffs.underlyings.size)
      return None
    }

    val variable = scheduledPayoffs.underlyings.head

    val fx = market.getFX(variable).orNull

    if (fx == null) {
      errorOutput(bond.id, "invalid fx underlying for FXMC1 model - " + variable + " in market " + market.paramset)
      return None
    }

    if (fx.currencyDom == bond.currency) {
      errorOutput(bond.id, "non-quanto model not supported by FXQtoMC1 model - " + variable)
      return None
    }

    val qtofx = market.getFX(bond.currency.code, fx.currencyDom.code, true).orNull

    if (qtofx == null) {
      errorOutput(bond.id, "invalid fx underlying for quanto model - " + qtofx.id + " in market " + market.paramset)
      return None
    }

    val mcmodel = mcengine(fx, qtofx).orNull

    if (mcmodel == null) {
      errorOutput(bond.id, "model name not found or model calibration error - FxQtoMc1f")
      return None
    }

    Some(FxMc1f(valuedate, mcmodel, scheduledPayoffs, fx, paths, triggers, FxMc1f.frontierFunction(bond, frontierPths), FxMc1f.paramRepository(bond), bond.id))
  }
}
