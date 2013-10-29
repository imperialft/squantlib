package squantlib.model.bond

import squantlib.util.Date
import squantlib.database.schemadefinitions.{Bond => dbBond}
import squantlib.util.{SimpleCache, FormulaParser}
import squantlib.pricing.model.PricingModel
import squantlib.schedule.call.Callabilities
import squantlib.schedule.payoff.{Payoff, Payoffs}
import squantlib.schedule.{ScheduledPayoffs, CalculationPeriod}
import squantlib.model.market.Market
import squantlib.pricing.model.NoModel
import org.jquantlib.currencies.Currency


class PriceableBond(
    db:dbBond, 
    scheduledPayoffs:ScheduledPayoffs,
	underlyings:List[String]) 
	extends BondModel(db, scheduledPayoffs, underlyings) 
    with Priceable 
    with Cloneable {

  var defaultModel:(Market, PriceableBond) => Option[PricingModel] = null
  
  var forceModel:Boolean = false
  
  var useCouponAsYield:Boolean = false
  
  var requiresCalibration:Boolean = false
  
  var modelCalibrated:Boolean = false
  
  var _market:Option[Market] = None
  
  var model:Option[PricingModel] = None
		
  override def toString:String = id
  
  /* 
   * Reset model
   */
  override def initializeModel(reCalibrate:Boolean = false):Unit = {
    if (reCalibrate) {calibrationCache.clear; modelCalibrated = false}
    
    model = if (isTerminated == Some(false) && scheduledPayoffs.isPriceable) livePayoffs match {
      case po if !po.isEmpty && !forceModel && po.isFixed => Some(NoModel(po))
      case _ => if (defaultModel == null) None else defaultModel(market.get, this)
    } else None
    
    cache.clear
    if (requiresCalibration && !modelCalibrated) { modelCalibrated = true; calibrateModel}
  }
  
  def reset(newMarket:Market, setter:(Market, BondModel) => Option[PricingModel]) = {
    val recalib = market match {case Some(m) => !m.valuedate.eq(newMarket.valuedate) case None => true}
    _market = Some(newMarket)
    defaultModel = setter
    initializeModel(recalib)
  }
	
  private def disp(name:String, f: => Any) = println(name + (" " * math.max(10 - name.size, 0)) + "\t" + (f match {
	  case null => "null"
	  case s:Option[Any] => s.getOrElse("None")
	  case s => s.toString
	}))
	
	
  def checkPayoffs:List[Boolean] = scheduledPayoffs.map{case (s, p, c) => 
	  p.isPriceable && 
	  c.isPriceable && 
	  (p.variables ++ c.variables).forall(underlyings.contains)}.toList
	  
	
  def show:Unit = {
	    disp("id", id)
	    disp("currency", currency.code)
	    disp("model", model match { case None => "Not defined" case Some(m) => m.getClass.getName})
	    disp("market", market match { case None => "Not defined" case Some(m) => m.paramset})
	    disp("underlyings", underlyings.mkString(" "))
	    disp("initial", underlyings.map(u => u + " -> " + db.fixingMap.getOrElse(u, "not fixed")).mkString(" "))
	    disp("current", market.collect{case mkt => underlyings.map(u => u + " -> " + mkt.getFixing(u).getOrElse("not fixed")).mkString(" ")}.getOrElse("no market"))
	    disp("termination", earlyTerminationDate.getOrElse("not terminated"))
	    println("Full schedule:")
	    println(scheduledPayoffs.toString)
	  }
	
	
  def showUnderlyingInfo:Unit = {
	  val eventDates:List[Date] = scheduledPayoffs.schedule.eventDates
	  getUnderlyings.foreach{
	    case (k, Some(u)) => println(k); u.show(eventDates)
	    case (k, None) => println(k); println("not found in market or market not calibrated")
	  }
	}  
  
  override def clone:PriceableBond = {
    val bond = new PriceableBond(db, scheduledPayoffs, underlyings) 
    bond.defaultModel = this.defaultModel
    bond.forceModel = this.forceModel
    bond.useCouponAsYield = this.useCouponAsYield
    bond.requiresCalibration = this.requiresCalibration
    bond.modelCalibrated = this.modelCalibrated
    bond._market = this._market
    bond.model = this.model
	calibrationCache.cache.foreach{case (a, b) => bond.calibrationCache.cache.update(a, b)}
	bond
  }
	
  /*
  * Creates clone of the same bond with date shifted by given days
  */
  override def dateShifted(shift:Int):PriceableBond = 
    PriceableBond.initialize(db, scheduledPayoffs.shifted(shift), underlyings, defaultModel, forceModel, useCouponAsYield, requiresCalibration, modelCalibrated, _market, model)

  /*
   * Creates clone of the same bond with trigger replaced with given triggers.
   */
  def triggerShifted(trig:List[List[Option[Double]]]):PriceableBond = {
    val newtrig = trig.size match {
      case s if s == trigger.size => trig
      case s if s < trigger.size => List.fill(trigger.size - trig.size)(List.empty) ++ trig
	  case s if s > trigger.size => trig takeRight trigger.size
	}
	val newSchedule = ScheduledPayoffs(schedule, payoffs, Callabilities(bermudan, newtrig, underlyings))
	PriceableBond.initialize(db, newSchedule, underlyings, defaultModel, forceModel, useCouponAsYield, requiresCalibration, modelCalibrated, _market, model)
  }
}

object PriceableBond {
  
  def apply(db:dbBond):Option[PriceableBond] = {
	val tbdfixings = try { Some(db.settingMap("tbd").toDouble)} catch {case _:Throwable => None}
	apply(db, tbdfixings)
  }
	
  def apply(db:dbBond, tbdfixing:Option[Double]):Option[PriceableBond] = 
    getScheduledPayoffs(db, tbdfixing).collect{case po => new PriceableBond(db, po, db.underlyingList)}
  
  def getScheduledPayoffs(db:dbBond, tbdfixing:Option[Double]):Option[ScheduledPayoffs] = {
	val schedule = db.schedule.orNull
	if (schedule == null) {return None}
	  
	val fixings:Map[String, Double] = db.getInitialFixings ++ tbdfixing.collect{case v => Map("tbd" -> v)}.getOrElse(Map.empty)
	  
	val coupon:Payoffs = Payoffs(db.fixedCoupon(fixings), schedule.size - 1).orNull
	if (coupon == null || coupon.size + 1 != schedule.size) {println(db.id + ": cannot initialize coupon"); return None}
	  
	val redemption = Payoff(db.fixedRedemprice(fixings)).orNull
	if (redemption == null) {println(db.id + ": cannot initialize redemption"); return None}
	  
	val underlyings:List[String] = db.underlyingList
		
	val bermudan:List[Boolean] = {
	  val bermlist = db.bermudanList(fixings, schedule.size)
		if (!bermlist.isEmpty && bermlist.takeRight(1).head) (bermlist.dropRight(1) :+ false) 
		else bermlist
	  }
	
	val trigger = db.triggerList(fixings, schedule.size)
	  
	val calls = Callabilities(bermudan, trigger, underlyings)
	if (calls == null) {println(db.id + ": cannot initialize calls"); return None}
	  
	val sp = ScheduledPayoffs.sorted(schedule, coupon :+ redemption, calls.fill(schedule.size))
	
	if (sp == null || sp.isEmpty) {
	  println(db.id + ": cannot initialize scheduled payoffs")
	  None}
	else Some(sp)
  }
  
  def initialize(
      db:dbBond, 
      scheduledPayoffs:ScheduledPayoffs,
      underlyings:List[String],
      defaultModel:(Market, PriceableBond) => Option[PricingModel] = null,
      forceModel:Boolean = false,
      useCouponAsYield:Boolean = false,
      requiresCalibration:Boolean = false,
      modelCalibrated:Boolean = false,
      _market:Option[Market] = None,
      model:Option[PricingModel] = None,
      calibrationCache:Option[SimpleCache] = None):PriceableBond = {
    
    val bond = new PriceableBond(db, scheduledPayoffs, underlyings)
    bond.defaultModel = defaultModel
    bond.forceModel = forceModel
    bond.useCouponAsYield = useCouponAsYield
    bond.requiresCalibration = requiresCalibration
    bond.modelCalibrated = modelCalibrated
    bond._market = _market
    bond.model = model
    calibrationCache match {
      case Some(cc) => cc.cache.foreach{case (a, b) => bond.calibrationCache.cache.update(a, b)}
      case _ =>
    }
	bond
  }
  
}

