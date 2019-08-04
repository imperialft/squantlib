package net.squantlib.model.bond

import net.squantlib.util.{Date, FixingInformation, FormulaParser, SimpleCache}
import net.squantlib.database.schemadefinitions.{Bond => dbBond}
import net.squantlib.pricing.model.PricingModel
import net.squantlib.schedule.call.{Callabilities, Callability}
import net.squantlib.schedule.payoff._
import net.squantlib.schedule.{CalculationPeriod, ScheduledPayoffs}
import net.squantlib.model.market.Market
import net.squantlib.pricing.model.NoModel
import org.jquantlib.currencies.Currency
import net.squantlib.util.DisplayUtils._


trait PriceableBond extends BondModel with Priceable {
  
  override val db:dbBond
  
  override val scheduledPayoffs:ScheduledPayoffs
  
  override val underlyings:List[String] 
  
  def copy(newdb:dbBond, newSchedule:ScheduledPayoffs, newuls:List[String]):PriceableBond
  
  override def copy:PriceableBond = copy(db, scheduledPayoffs, underlyings)

  //var defaultModel:(Market, PriceableBond) => Option[PricingModel] = null

  
  var forceModel:Boolean = false
  
  var useCouponAsYield:Boolean = false
  
  var requiresCalibration:Boolean = false
  
  var modelCalibrated:Boolean = false
  
  var _market:Option[Market] = None
  
  var model:Option[PricingModel] = None

  var defaultMcPaths:Option[Int] = None
    
  override def toString:String = id
  
  /* 
   * Reset model
   */
  override def initializeModel(reCalibrate:Boolean, modelName:String):Unit = {
    if (reCalibrate) {
      calibrationCache.clear
      modelCalibrated = false
    }
    
    model = if (isTerminated == Some(false) && scheduledPayoffs.isPriceable) livePayoffs match {
      case _ if modelName != null => models.get(modelName) match {
        case Some(mo) => 
          currentModelName = modelName
          mo(market.get, this)
        case _ => None
      }
      case po if !po.isEmpty && !forceModel && po.isFixed => Some(NoModel(po))
      case _ if (defaultModel == null) => None
      case _ => 
        currentModelName = defaultModelName
        defaultModel(market.get, this)
    } else None
    
    cache.clear
    if (requiresCalibration && !modelCalibrated) {
      modelCalibrated = true
      calibrateModel
    }
  }
  
  override def reset(newMarket:Market, setter:(Market, BondModel) => Option[PricingModel], modelName:String) = {
    val recalib = market.collect{case m => !m.valuedate.eq(newMarket.valuedate)}.getOrElse(true)
    _market = Some(newMarket)
    scheduledPayoffs.updateFutureFixings(newMarket.valuedate)
    initializeEarlyTermination
    models = models + (defaultModelName -> setter)
    currentModelName = defaultModelName
    //defaultModel = setter
    initializeModel(recalib, modelName)
  }
  
  private def disp(name:String, f: => Any) = standardOutput(id, name + (" " * math.max(10 - name.size, 0)) + "\t" + (f match {
    case null => "null"
    case s:Option[Any] => s.getOrElse("None")
    case s => s.toString
  }))
  
  
  def checkPayoffs:List[Boolean] = scheduledPayoffs.map{case (s, p, c) => 
    p.isPriceable && 
    c.isPriceable && 
    (p.variables ++ c.variables).forall(underlyings.contains)}.toList
    
  
  def transferSettings[T <: PriceableBond](bond:T):Unit = {
    //bond.defaultModel = this.defaultModel
    bond.defaultModelName = this.defaultModelName
    bond.currentModelName = this.currentModelName
    bond.models = this.models
    bond.forceModel = this.forceModel
    bond.useCouponAsYield = this.useCouponAsYield
    bond.requiresCalibration = this.requiresCalibration
    bond.modelCalibrated = this.modelCalibrated
    bond._market = this._market
    bond.model = this.model
    bond.fixingInformation = this.fixingInformation
    bond.defaultMcPaths = this.defaultMcPaths
    this.mcPaths match {
      case Some(n) => bond.setMcPaths(n, false)
      case _  => {}
    }

    calibrationCache.cache.foreach{case (a, b) => bond.calibrationCache.cache.update(a, b)}
  }
    
  def clearFixings:Unit = {
    scheduledPayoffs.clearFixings

//    scheduledPayoffs.foreach{case (s, p, c) =>
//      p.clearFixings
//      c.clearFixings
//    }

//    payoffs.foreach{
//      case p:PutDIAmericanPayoff => p.knockedIn = false
//      case p:PutDIPayoff => p.knockedIn = false
//      case p:CallUIPayoff => p.fixedPrice = None
//      case p:RangeForwardPayoff => p.fixedPrice = None
//      case p => {}
//    }
  }
  
  /*
  * Creates clone of the same bond with date shifted by given days
  */
  override def dateShifted(shift:Int):PriceableBond = 
    copy(db, scheduledPayoffs.shifted(shift), underlyings)

  /*
   * Creates clone of the same bond with trigger replaced with given triggers.
   */
  def triggerShifted(trig:List[List[Option[Double]]]):PriceableBond = {
    val newtrig:List[List[Option[Double]]] = trig.size match {
      case s if s == trigger.size => trig
      case s if s < trigger.size => List.fill(trigger.size - trig.size)(List.empty) ++ trig
    case s if s > trigger.size => trig takeRight trigger.size
    }
//  val newSchedule = ScheduledPayoffs(schedule, payoffs, Callabilities(bermudan, newtrig, underlyings))
    val newCalls:List[Callability] = (calls, newtrig).zipped.map{case (c, t) => Callability(
      bermudan = c.bermudan,
      triggers = underlyings.zip(t).collect{case (k, Some(v)) => (k, v.getDecimal(k))}.collect{case (k, Some(v)) => (k, v)}.toMap,
      triggerUp = c.triggerUp,
      targetRedemption = c.targetRedemption,
      forward = c.forward,
      bonusAmount = c.bonusAmount,
      removeSatisfiedTriggers = c.removeSatisfiedTriggers,
      inputString = Map.empty,
      accumulatedPayments = c.accumulatedPayments,
      simulatedFrontier = Map.empty
    )}.toList
    
    val newSchedule = ScheduledPayoffs.noFixing(schedule, payoffs, Callabilities(newCalls))

    copy(db, newSchedule, underlyings)
  }
  
  
  def show:Unit = {
      disp("id", id)
      disp("currency", currency.code)
      disp("model", model match { case None => "Not defined" case Some(m) => m.getClass.getName})
      disp("market", market match { case None => "Not defined" case Some(m) => m.paramset + " : " + m.valuedate})
      disp("underlyings", underlyings.mkString(" "))
//      disp("initial", underlyings.map(u => u + " -> " + db.fixingMap.getOrElse(u, "not fixed")).mkString(" "))
      disp("initial", underlyings.map(u => u + " -> " + fixingInformation.initialFixing.getOrElse(u, "not fixed")).mkString(" "))
      disp("current", market.collect{case mkt => underlyings.map(u => u + " -> " + mkt.getFixing(u).getOrElse("not fixed")).mkString(" ")}.getOrElse("no market"))
      disp("termination", earlyTerminationDate.getOrElse("not terminated"))
      standardOutput(id, "Full schedule:")
      standardOutput(id, scheduledPayoffs.toString)
    }
  
  
  def showUnderlyingInfo:Unit = {
    val eventDates:List[Date] = scheduledPayoffs.schedule.eventDates
    getUnderlyings.foreach{
      case (k, Some(u)) => standardOutput(k); u.show(eventDates)
      case (k, None) => standardOutput(k); errorOutput("not found in market or market not calibrated")
    }
  }  
  
}


