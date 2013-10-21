package squantlib.model.bond

import org.jquantlib.currencies.Currency
import squantlib.util.Date
import org.jquantlib.time.{Period => qlPeriod, TimeUnit, Calendar, Frequency}
import org.jquantlib.termstructures.Compounding
import org.jquantlib.daycounters.{Actual365Fixed, DayCounter}
import squantlib.database.DB
import squantlib.database.schemadefinitions.{Bond => dbBond}
import squantlib.schedule.payoff._
import squantlib.util.initializer.Currencies
import squantlib.util.JsonUtils._
import squantlib.util.{SimpleCache, FormulaParser}
import squantlib.pricing.model.PricingModel
import squantlib.math.solver._
import squantlib.math.financial.{BondYield, Duration}
import squantlib.schedule.call.Callabilities
import squantlib.schedule.payoff.{Payoff, Payoffs}
import squantlib.schedule.{ScheduledPayoffs, CalculationPeriod}
import squantlib.pricing.model.NoModel
import squantlib.model.Market
import squantlib.model.rates.DiscountCurve
import squantlib.model.asset.{AnalyzedAsset, Underlying}
import squantlib.util.UnderlyingParser
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import scala.collection.JavaConversions._
import scala.collection.breakOut
import scala.collection.mutable.ArrayBuffer


/**
 * Bond class with enclosed risk analysis functions.
 */
case class Bond(
		db:dbBond, 
		scheduledPayoffs:ScheduledPayoffs,
		underlyings:List[String],
		var defaultModel:(Market, Bond) => Option[PricingModel] = null,
		var forceModel:Boolean = false,
		var useCouponAsYield:Boolean = false,
		var requiresCalibration:Boolean = false,
		var modelCalibrated:Boolean = false,
		var _market:Option[Market] = None,
		var model:Option[PricingModel] = None
		) extends AnalyzedAsset {
	
	/*
	 * Basic access functions
	 */
	def schedule = scheduledPayoffs.schedule
	
	def payoffs = scheduledPayoffs.payoffs
	
	def calls = scheduledPayoffs.calls
	
	def coupon = scheduledPayoffs.coupon
  
	override val id = db.id
	
	override val assetID = "PRICE"
	
	def issueDate:Date = schedule.head.startDate
	
	def isIssuedOn(d:Date):Boolean = d ge issueDate
	
	def isIssued:Option[Boolean] = valueDate.collect{case d => isIssuedOn(d)}
	
	def scheduledMaturity:Date = schedule.last.endDate
	
	def bermudan:List[Boolean] = calls.bermudans
	
	def trigger:List[List[Option[Double]]] = calls.triggerValues(underlyings)
	
	def nominal:Option[Double] = db.nominal
	
	val currency:Currency = Currencies(db.currencyid).orNull
	
	def denomination:Option[Double] = db.denomination
	
	def period:qlPeriod = (db.coupon_freq collect { case f => new qlPeriod(f, TimeUnit.Months)}).orNull

	def calendar:Calendar = db.calendar
	
	def issuePrice:Option[Double] = db.issueprice
	
	def call:String = db.call
	
	def initialFX:Double = db.initialfx
	
	def issuer:String = db.issuerid
	
	def settings:JsonNode = db.settingsJson
	
	def isFixedRateBond = payoffs.underlyings.size == 0
	
	def redemption:Payoff = {
	  val abslegs = scheduledPayoffs.filter{case (s, p, t) => s.isAbsolute}
	  val finalleg = if (abslegs.size > 0) abslegs.maxBy{case (s, p, t) => s.paymentDate}
	  				else scheduledPayoffs.maxBy{case (s, p, t) => s.paymentDate}
	  finalleg._2
	}
	
	def fixedRedemptionAmount:Option[Double] = {
	  val amount = redemption.price
	  if (amount.isNaN || amount.isInfinity) None else Some(amount)
	}
	
	def isScheduleMaturedOn(d:Date):Boolean = d ge scheduledMaturity
	
	def isScheduleMatured:Option[Boolean] = valueDate.collect { case vd => isScheduleMaturedOn(vd)}
	
	lazy val (earlyTerminationPeriod:Option[CalculationPeriod], earlyTerminationAmount:Option[Double]) = 
	  scheduledPayoffs.triggeredDate.collect{case (p, a) => (Some(p), Some(a))}.getOrElse((None, None))
	
	lazy val earlyTerminationDate:Option[Date] = earlyTerminationPeriod.collect{case p => p.paymentDate}
	
	def isEarlyTerminatedOn(d:Date):Boolean = earlyTerminationDate.collect{case dd => d ge dd}.getOrElse(false)
	  
	def isEarlyTerminated:Option[Boolean] = valueDate.collect{case d => isEarlyTerminatedOn(d)}
	
	def isTerminatedOn(d:Date):Boolean = isScheduleMaturedOn(d) || isEarlyTerminatedOn(d)
	  
	def isTerminated:Option[Boolean] = valueDate.collect{case d => isScheduleMaturedOn(d) || isEarlyTerminatedOn(d)}
	
	def terminationDate:Date = earlyTerminationDate.getOrElse(scheduledMaturity)
	
	override val assetStartDate = Some(issueDate)
	
	override val assetEndDate = Some(terminationDate)
	
	def isAlive(d:Date):Option[Boolean] = valueDate.collect{case d => isAliveOn(d)}
	
	def getUnderlyings:Map[String, Option[Underlying]] = market match {
	  case None => underlyings.map(u => (u, None)) (collection.breakOut)
	  case Some(mkt) => underlyings.map(u => (u, Underlying(u, mkt))) (collection.breakOut)
	}
	
	/*
	 * Creates clone of the same bond (shallow copy)
	 */
	override def clone:Bond = {
	  val bond = new Bond(db, scheduledPayoffs, underlyings, defaultModel, forceModel, useCouponAsYield, requiresCalibration, modelCalibrated, _market, model) 
	  calibrationCache.cache.foreach{case (a, b) => bond.calibrationCache.cache.update(a, b)}
	  bond
	}
	
	/*
	 * Creates clone of the same bond with date shifted by given days
	 */
	def dateShifted(shift:Int):Bond = 
	  new Bond(db, scheduledPayoffs.shifted(shift), underlyings, defaultModel, forceModel, useCouponAsYield, requiresCalibration, modelCalibrated, _market, model)

	/*
	 * Creates clone of the same bond with trigger replaced with given triggers.
	 */
	def triggerShifted(trig:List[List[Option[Double]]]):Bond = {
	  val newtrig = trig.size match {
	    case s if s == trigger.size => trig
	    case s if s < trigger.size => List.fill(trigger.size - trig.size)(List.empty) ++ trig
	    case s if s > trigger.size => trig takeRight trigger.size
	  }
	  val newSchedule = ScheduledPayoffs(schedule, payoffs, Callabilities(bermudan, newtrig, underlyings))
	  new Bond(db, newSchedule, underlyings, defaultModel, forceModel, useCouponAsYield, requiresCalibration, modelCalibrated, _market, model)
	}
	
	
	def market:Option[Market] = _market
	
	def market_= (newMarket:Market) = {
	  val recalib = market.isEmpty || !market.get.valuedate.eq(newMarket.valuedate) 
	  _market = Some(newMarket)
	  initializeModel(recalib)
	}
	
	def valueDate:Option[Date] = market.collect{case mkt => mkt.valuedate}
	
	/* 
	 * Reset model
	 */
	def initializeModel(reCalibrate:Boolean = false):Unit = {
	  if (reCalibrate) {calibrationCache.clear; modelCalibrated = false}
	  
	  model = if (isTerminated == Some(false) && scheduledPayoffs.isPriceable) livePayoffs match {
	    case po if !po.isEmpty && !forceModel && po.isFixed => Some(NoModel(po))
	    case _ => if (defaultModel == null) None else defaultModel(market.get, this)
	  } else None
	  
	  cache.clear
	  if (requiresCalibration && !modelCalibrated) { modelCalibrated = true; calibrateModel}
	}
	
	
	def reset(newMarket:Market, setter:(Market, Bond) => Option[PricingModel]) = {
	  val recalib = market match {case Some(m) => !m.valuedate.eq(newMarket.valuedate) case None => true}
	  _market = Some(newMarket)
	  defaultModel = setter
	  initializeModel(recalib)
	}
	
	/* 
	 * True if necessary to run calibrateModel to get accurate price.
	 */
	def calibrateModel = model match {
	  case Some(m) => {model = Some(m.calibrate); cache.clear}
	  case None => {}
	}
	
	/*
	 * Cache to store temporary values (currently used for spot and forward coupons)
	 */
	val cache = new SimpleCache
	val calibrationCache = new SimpleCache
	def getCalibrationCache(k:String):Option[Any] = calibrationCache.getAny(k)	
	
	/*	
	 * Returns "live" payment schedules
	 * 	@returns element 1: Schedule containing legs with payment date after market value date or specified value date.
	 * 			element 2: Payoffs containing legs with payment dates after market value date or specified value date.
	 */
	def livePayoffs:ScheduledPayoffs = valueDate.collect {case d => livePayoffs(d)}.getOrElse(ScheduledPayoffs.empty)

	def livePayoffs(vd:Date):ScheduledPayoffs = {
	  val p = (earlyTerminationDate,earlyTerminationAmount) match {
	    case (Some(d), _) if vd ge d => ScheduledPayoffs.empty
	    case (Some(d), Some(a)) => scheduledPayoffs.after(vd).called(d, a, calendar, db.paymentAdjust).withValueDate(vd)
	    case _ => scheduledPayoffs.after(vd).withValueDate(vd)
	  }
	  p
	}
	
	def liveCoupons:ScheduledPayoffs = livePayoffs.filtered{case (period, _, _) => !period.isAbsolute}
	
	def liveCoupons(vd:Date):ScheduledPayoffs = livePayoffs(vd).filtered{case (period, _, _) => !period.isAbsolute}
	
	def allPayoffs:ScheduledPayoffs = scheduledPayoffs
	
	/*	
	 * Returns "live" triggers
	 * 	@returns list of remaining triggers
	 */
	
	def liveCallabilities:Callabilities = livePayoffs.calls
	
	def liveTriggers:List[List[Option[Double]]] = livePayoffs.calls.triggerValues(underlyings)
	
	def liveTriggers(vd:Date):List[List[Option[Double]]] = livePayoffs(vd).calls.triggerValues(underlyings)
	
	/*	
	 * Returns "live" bermudan call options
	 * 	@returns list of remaining bermudan calls
	 */
	def liveBermudans:List[(CalculationPeriod, Boolean)] = livePayoffs.map{case (d, _, c) => (d, c.isBermuda)} (collection.breakOut)
		
	def liveBermudans(vd:Date):List[(CalculationPeriod, Boolean)] = livePayoffs(vd).map{case (d, _, c) => (d, c.isBermuda)}(collection.breakOut)
	
	/*	
	 * Returns discount curve.
	 * 	@returns discount curve created from either pre-set or specified market
	 */
	def discountCurve:Option[DiscountCurve] = market.flatMap(m => m.getDiscountCurve(currency, issuer))
	
	/*	
	 * Returns discount curve.
	 * 	@returns discount curve created from either pre-set or specified market
	 */
	def discountFactors:Option[List[(Date, Double)]] = (discountCurve, valueDate) match {
	  case (Some(curve), Some(vd)) => Some(schedule.paymentDates.withFilter(_ gt vd).map(d => (d, curve(d))))
	  case _ => None
	}

	/*	
	 * Returns coupons fixed with current spot market (not forward!). 
	 */
	def spotFixedRates:List[(CalculationPeriod, Double)] = cache.getOrUpdate("SPOTFIXEDRATES",
	    livePayoffs.map{case (d, p, _) => (d, market match { case Some(mkt) => p.price(mkt) case None => Double.NaN})}(collection.breakOut)
	  )
	  
	def spotFixedRates(vd:Date):List[(CalculationPeriod, Double)] = spotFixedRates.filter{case (p, d) => (p.paymentDate gt vd)}
	  
	def spotFixedAmount:List[(Date, Double)] = spotFixedRates.map{case (period, rate) => (period.paymentDate, rate * period.dayCount)}
	
	def spotFixedAmount(vd:Date):List[(Date, Double)] = spotFixedAmount.filter{case (d, _) => (d gt vd)}
	  
	def spotFixedRatesAll:List[(CalculationPeriod, Double)] = cache.getOrUpdate("SPOTFIXEDRATESALL",
	    allPayoffs.map{case (d, p, _) => (d, market match { case Some(mkt) => p.price(mkt) case None => Double.NaN})} (collection.breakOut)
	  )
	  
	def spotFixedAmountAll:List[(Date, Double)] = spotFixedRatesAll.map{case (period, rate) => (period.paymentDate, rate * period.dayCount)}
	
    def spotCashflowDayfrac(dc:DayCounter):List[(Double, Double)] = spotFixedAmount.map{
      case (payday, amount) => (Date.daycount(valueDate.get, payday, dc), amount)}
	
	/*	
	 * Returns dirty price of the bond. (ie. including accrued interest)
	 */
	def dirtyPrice:Option[Double] = 
	  if(!scheduledPayoffs.isPriceable) {println(id + " : invalid payoff or trigger"); None}
	  else (earlyTerminationDate, valueDate) match {
	    case (Some(td), Some(vd)) if td le vd => println(id + " : terminated on " + td); None
	    case _ => (model, discountCurve) match {
	      case (Some(m), Some(c)) => m.price(c)
	      case (Some(m), None) => println(id + " : missing discount curve"); m.price
	      case _ => println(id + " : missing model"); None
	}}
	
	override def isPriced: Boolean = dirtyPrice.isDefined
	
	override def latestPriceLocalCcy: Option[Double] = dirtyPrice
	
	/*	
	 * Returns clean price of the bond (ie. Dirty price - accrued coupon)
	 */
	def cleanPrice:Option[Double] = (dirtyPrice, accruedAmount) match { 
	  case (Some(d), Some(a)) => Some(d - a) 
	  case _ => None
	}
	
	/*	
	 * Returns accrued coupon.
	 */
	def accruedAmount:Option[Double] = market.flatMap(mkt => 
	  if (issueDate ge mkt.valuedate) Some(0.0)
	  else if (coupon isEmpty) Some(0.0)
	  else livePayoffs.filter{case (d, p, _) => (d.isCurrentPeriod(mkt.valuedate) && !d.isAbsolute)} match {
	    case pos if pos.isEmpty => None
	    case pos => Some(pos.map{case (d, p, _) => (d.accrued(mkt.valuedate)) * p.price(mkt) }.sum)
	  })
	  
	/*	
	 * Returns dirty price of the bond using the model parameters.
	 */
	def modelPrice:Option[Double] = (model, discountCurve) match {
	  case (Some(m), Some(c)) => m.modelPrice(c)
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
        lowRange:Double = 0.001):List[Option[Double]] = 
      
      if (market.isEmpty || dirtyPrice.isEmpty) List.fill(underlyings.size)(None)
      
      else {
        val mkt = market.get
        val shift = this.valueDate.get.sub(vd).toInt
        val bond = this.dateShifted(this.valueDate.get.sub(vd).toInt)
        
        underlyings.map(ul => {
          if (ul.size != 6 || ul.takeRight(3) == "JPY") None
            val ccy = ul take 3
            
			def priceFromFXmult(y:Double):Double = {
              bond.market = mkt.fxShifted(Map(ccy -> y))
              if (paths > 0) bond.model.collect{case m => m.mcPaths = paths}
              bond.dirtyPrice.getOrElse(Double.NaN)
            }
            
            val priceformula = (y:Double) => (priceFromFXmult(y) - target)
            val mult = solver.solve(priceformula, lowRange, highRange, accuracy, maxIteration)
            mult.collect{case m => mkt.fx(ccy, "JPY").getOrElse(Double.NaN) / m}
          })
        }
    
    def fxFrontiers:List[List[Option[Double]]] = fxFrontiers(1.00, 0.001, 20)
      
    def fxFrontiers(target:Double, accuracy:Double, maxIteration:Int, paths:Int = 0):List[List[Option[Double]]] = {
      val valuedates = livePayoffs
    		  	.zipWithIndex
    		  	.filter{case (c, _) => c._3.isBermuda && !c._3.isTrigger}
      			.map{case (c, index) => (c._1.paymentDate, index)}
      			.sortBy{case (date, _) => date}
      			.reverse
      
      val tempTrigger = ArrayBuffer(liveTriggers:_*)
      println("FX frontiers : " + id)
      
      valuedates.foreach{case (vd, index) => 
        val tempBond = triggerShifted(tempTrigger.toList)
        tempTrigger(index) = tempBond.fxFrontier(1.00, accuracy, maxIteration, vd, paths)
        println(index + " : " + tempTrigger(index).mkString(","))
      }
      tempTrigger.toList
    }
	
	/*	
	 * Returns next bermudan callable date.
	 */
	def nextBermudan:Option[Date] = {
	  val bermdates = liveBermudans.filter{case (d, c) => c}.map{case (d, c) => d.paymentDate}
	  if (bermdates.isEmpty) None else Some(bermdates.min)
	}
	
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
	 * Continuous rate at which MtM exceeds 100% at next call date.
	 */
	def nextRateFrontier:Option[Double] = nextBermudan.flatMap{ case d => getYield(1.0, new Actual365Fixed, Compounding.Continuous, null, 0.00001, 20, d) }

	/*	
	 * Returns bond yield.
	 * @param comp Compounding rate, as one of the following
	 * 		"None" => Discounting is not taken into account : ZC = 1.0
	 * 		"Simple" => No compounding : ZC = 1 / rt
	 * 		"Compounded" => Standard compounding: ZC = 1 / (1+r/f)^tf 
	 * 		"Continuous" => 
	 */
	def getYield(comp:Compounding):Option[Double] = getYield(comp, Frequency.Annual)
	
	def getYield(comp:Compounding, freq:Frequency):Option[Double] = getYield(comp, freq, new Actual365Fixed, 0.00001, 20)
	
    def getYield(comp:Compounding, freq:Frequency, dc:DayCounter, accuracy:Double, maxIteration:Int):Option[Double] = {
      val result = if (useCouponAsYield) {
          val cashflows = spotCashflowDayfrac(dc)
          accruedAmount.collect{case acc => (cashflows.unzip._2.sum - acc - 1.0) / cashflows.unzip._1.max}} 
        else dirtyPrice.flatMap{case p => getYield(p, dc, comp, freq, accuracy, maxIteration)}
      
      if (result == Some(Double.NaN)) None else result
	}
	
    def getYield(price:Double, comp:Compounding, freq:Frequency, vd:Date):Option[Double] = 
      getYield(price, new Actual365Fixed, comp, freq, 0.0001, 20, vd)
	
    def getYield(price:Double, dc:DayCounter, comp:Compounding, freq:Frequency, accuracy:Double, maxIteration:Int):Option[Double] = 
      valueDate.flatMap{ case vd => getYield(price, spotCashflowDayfrac(dc), comp, freq, accuracy, maxIteration, vd)}
	
    def getYield(price:Double, dc:DayCounter, comp:Compounding, freq:Frequency, accuracy:Double, maxIteration:Int, vd:Date):Option[Double] = 
      getYield(price, spotCashflowDayfrac(dc), comp, freq, accuracy, maxIteration, vd)
      
    def getYield(price:Double, cashflows:List[(Double, Double)], comp:Compounding, freq:Frequency, accuracy:Double, maxIteration:Int, vd:Date):Option[Double] = {
      val result = 
        if (cashflows isEmpty) None
        else if (useCouponAsYield) accruedAmount.collect{case acc => BondYield.asAverageCoupon(cashflows, acc)}
        else comp match {
          case Compounding.Simple => BondYield.solveNoCompounding(price, cashflows, accuracy, maxIteration)
          case Compounding.Compounded | Compounding.SimpleThenCompounded => BondYield.solveCompounded(price, cashflows, freq.toInteger, accuracy, maxIteration)
          case Compounding.Continuous => BondYield.solveContinuous(price, cashflows, accuracy, maxIteration)
          case Compounding.None => accruedAmount.collect{case acc => BondYield.solveNoRate(price, cashflows, acc)}
          case _ => None
        }
      if (result.collect{case r => r.isNaN || r.isInfinity}.getOrElse(false)) None else result
    }
	
	def modelPriceJpy:Option[Double] = (modelPrice, fxjpy, db.initialfx) match { 
	  case (Some(p), Some(fx), init) if init > 0 => Some(p * fx / init)
	  case (Some(p), Some(fx), init) if isIssued == Some(false) => Some(p)
	  case _ => None
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
	 * Returns spot FX rate against JPY
	 */
	def fxjpy:Option[Double] = market.flatMap (mkt => mkt.fx(currency.code, "JPY"))
	
	/*	
	 * Returns JPY dirty price defined as price x FX/FX0, where FX0 = FX as of issue date.
	 */
	def dirtyPriceJpy:Option[Double] = (dirtyPrice, fxjpy, db.initialfx) match { 
	  case (Some(p), Some(fx), init) if init > 0 => Some(p * fx / init)
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
	 * Returns JPY accrued amount defined as accrued x FX/FX0, where FX0 = FX as of issue date.
	 */
	def accruedAmountJpy:Option[Double] = (accruedAmount, fxjpy, db.initialfx) match { 
	  case (Some(p), Some(fx), init) if init > 0 => Some(p * fx / init)
	  case _ => None
	}
	
    
	/*	Returns each bond yield.
	 */
	def yieldNoCompounding:Option[Double] = getYield(Compounding.Simple)
	
	def yieldContinuous:Option[Double] = getYield(Compounding.Continuous)
	
	def yieldSemiannual:Option[Double] = getYield(Compounding.Compounded, Frequency.Semiannual)
	
	def yieldAnnual:Option[Double] = getYield(Compounding.Compounded, Frequency.Annual)
	
	def yieldSimple:Option[Double] = getYield(Compounding.None, Frequency.Annual)
	
	/*	Returns yield at which bond price becomes 100% (if any)
	 * @param comp Compounding rate, as one of the following
	 * 		"None" => Not applicable
	 * 		"Simple" => No compounding : ZC = 1 / rt
	 * 		"Compounded" => Standard compounding: ZC = 1 / (1+r/f)^tf 	
	 * 		"Continuous" => 
	 */
	def parMtMYield:Option[Double] = getYield(1.0, new Actual365Fixed, Compounding.Continuous, null, 0.00001, 20)
	
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
	
	/*	
	 * Internal Rate of Return, defined to be the same as annually compounded yield.
	 */
    def irr:Option[Double] = irr(new Actual365Fixed, 0.00001, 20)
    def irr(dc:DayCounter, accuracy:Double, maxIteration:Int):Option[Double] = yieldAnnual
    
    /*
     * Yield value of a basis point. The yield value of a one basis point change
     * in price is the derivative of the yield with respect to the price
     */
    def yieldValueBasisPoint:Option[Double] = (dirtyPrice, modifiedDuration) match {
      case (Some(p), Some(dur)) => Some(1.0 / (-p * dur))
      case _ => None
    }
    
	/*	
	 * Returns Macauley duration defined as Sum {tV} / Sum{V}
	 */
    def macaulayDuration:Option[Double] = if (isTerminated == Some(true)) Some(0.0) else discountCurve.flatMap{case curve => 
      val mac = Duration.macaulay(spotCashflowDayfrac(new Actual365Fixed), (d:Double) => curve(d * 365.25))
      if (mac.isNaN || mac.isInfinity) None else Some(mac)
    }
    
	/*	
	 * Returns modified duration defined as Macauley duration / (1 + yield / freq)
	 */
	def modifiedDuration:Option[Double] = modifiedDuration(Compounding.Compounded, Frequency.Annual)
	def modifiedDuration(comp:Compounding, freq:Frequency):Option[Double] = if (isTerminated == Some(true)) Some(0.0) 
	  else macaulayDuration.flatMap { case dur =>
	    comp match {
	      case Compounding.Continuous => Some(dur)
	      case Compounding.Compounded | Compounding.SimpleThenCompounded => getYield(comp, freq).collect{case y => dur / (1.0 + y / freq.toInteger.toDouble)}
	      case _ => None
	  }
	}
	
	/*	
	 * Returns effective duration defined as 1bp rate delta * 10000
	 */
	def effectiveDuration:Option[Double] = if (isTerminated == Some(true)) Some(0.0) else rateDelta(-0.0001).collect{case d => d * 10000} // TO BE COMPUTED AS RATE DELTA
	
	/*	
	 * List of underlying currencies
	 */
	
	def currencyList:Set[String] = {
	  val ulset:Set[String] = underlyings.toSet
	  val ulccys:Set[String] = ulset.map(ul => {
	    UnderlyingParser.getParser(ul).collect{case p => p.getType} match {
	    case Some(UnderlyingParser.typeCcy) => Set(ul)
	    case Some(UnderlyingParser.typeFX) => Set(ul take 3, ul takeRight 3)
	    case _ => Set.empty
	  }}).flatten
	  ulccys + currency.code
	}

	def greek(target:Bond => Option[Double], operation:Market => Option[Market]):Option[Double] = market.flatMap { case mkt =>
	  val initprice = target(this)
	  val newBond = this.clone
	  val newmkt = operation(mkt).orNull
	  if (newmkt == null) {return None}
	  newBond.market = newmkt
	  val newprice = target(newBond)
	  (initprice, newprice) match { 
	    case (Some(i), Some(n)) if !i.isNaN && !n.isNaN && !i.isInfinity && !n.isInfinity => Some(n - i) 
	    case _ => None }
	}
	
	/*	
	 * Returns rate delta
	 */
	def rateDelta(shift:Double):Option[Double] = rateDelta(currency.code, shift)
	
	def rateDelta(ccy:String, shift:Double):Option[Double] = rateDelta((b:Bond) => b.modelPrice, Map(ccy -> shift))
		
	def rateDelta(target:Bond => Option[Double], shift:Map[String, Double]):Option[Double] = greek(target, (m:Market) => Some(m.rateShifted(shift)))

	
	/*	
	 * Returns rate delta for all involved currencies.
	 */
	def rateDeltas(shift:Double):Map[String, Double] = currencyList.map(f => (f, rateDelta(f, shift))).collect{case (a, Some(b)) => (a, b)} (breakOut)

	/*	
	 * Return FX delta defined as MtM change when multiplying FX by given amount
	 */
	def fxDelta(ccy:String, mult:Double):Option[Double] = fxDelta((b:Bond) => b.modelPrice, Map(ccy -> mult))
	
	def fxDelta(target:Bond => Option[Double], mult:Map[String, Double]):Option[Double]	= greek(target, (m:Market) => Some(m.fxShifted(mult)))
	
	/*	
	 * Returns FX delta for all involved currencies.
	 */
	def fxDeltas(mult:Double):Map[String, Double] = (currencyList - currency.code).map(ccy => ("USD" + ccy, fxDelta((b:Bond) => b.modelPrice, Map(ccy -> mult)))).collect{case (a, Some(b)) => (a, b)}.toMap
		
	/*	
	 * Returns FX delta on JPY bond price.
	 */
	def fxDeltaJpy(mult:Double):Map[String, Double] = (currencyList - "JPY").map(f => 
	  (f + "JPY", fxDelta((b:Bond) => b.modelPriceJpy, Map(f -> 1/mult)))).collect{case (a, Some(b)) => (a, b)} (breakOut)

	  
	/*	
	 * Returns delta of 1 yen change in FX on JPY price.
	 */
	def fxDeltaOneJpy:Map[String, Double] = market match {
	  case None => Map.empty
	  case Some(mkt) => (currencyList - "JPY").map(ccy => mkt.fx(ccy, "JPY") match {
	      case Some(fx) => (ccy + "JPY", fxDelta((b:Bond) => b.modelPriceJpy, Map(ccy -> fx/(fx+1))))
	      case None => (ccy + "JPY", None)
	    }).collect{case (a, Some(b)) => (a, b)}(breakOut)}
	    
	
	/*	
	 * List of FX underlyings
	 */
	def fxList:Set[String] = livePayoffs.payoffs.underlyings.filter(
	  c => ((c.size == 6) && (Currencies contains (c take 3)) && (Currencies contains (c takeRight 3))))

	/*	
	 * Returns rate vega
	 */
	def fxVegas(addvol:Double):Map[String, Double] = fxList.map(fx => (fx, fxVega(fx, addvol))).collect{case (a, Some(b)) => (a, b)} (breakOut)
	
	def fxVega(ccypair:String, addvol:Double):Option[Double] = fxVega((b:Bond) => b.modelPrice, Map(ccypair -> addvol))
	
	def fxVega(target:Bond => Option[Double], addvol:Map[String, Double]):Option[Double] = greek(target, (m:Market) => Some(m.fxVolShifted(addvol)))
	  
	
	/*	
	 * Returns delta for any underlying
	 */
	def underlyingDelta(id:String, shift:Double):Option[Double] = greek((b:Bond) => b.modelPriceJpy, (m:Market) => m.underlyingShifted(id, shift))
	
	def underlyingDeltas(shift:Double):Map[String, Option[Double]] = {
	  val modifieduls = underlyings.map(u => if(FormulaParser.isFX(u) && u.take(3) == "JPY") u.takeRight(3) + u.take(3) else u)
	  val uls = modifieduls ++ currencyList.filter(c => c != "JPY" && !modifieduls.contains(c + "JPY")).map(_ + "JPY")
	  uls.map(ul => (ul, underlyingDelta(ul, shift)))(collection.breakOut)
	}
	
	def underlyingVega(id:String, shift:Double):Option[Double] = greek((b:Bond) => b.modelPriceJpy, (m:Market) => m.underlyingVolShifted(id, shift))
	
	def underlyingVegas(shift:Double):Map[String, Option[Double]] = {
	  val modifieduls = underlyings.map(u => if(FormulaParser.isFX(u) && u.take(3) == "JPY") u.takeRight(3) + u.take(3) else u)
	  val uls = modifieduls ++ currencyList.filter(c => c != "JPY" && !modifieduls.contains(c + "JPY")).map(_ + "JPY")
	  uls.map(ul => (ul, underlyingVega(ul, shift))) (collection.breakOut)
	}
	
	/*
     * Cash-flow convexity
     * The convexity of a string of cash flows is defined as {@latex[ C = \frac{1}{P} \frac{\partial^2 P}{\partial y^2} } where
     * {@latex$ P } is the present value of the cash flows according to the given IRR {@latex$ y }.
     */
	
    def formulaConvexity(comp:Compounding, freq:Frequency = Frequency.Annual):Option[Double] = discountCurve.flatMap { 
	  case discount => 
	    val cashflows = spotCashflowDayfrac(new Actual365Fixed)
	    val discounter = (d:Double) => discount(d * 365.25)
	    comp match {
	      case Compounding.Simple => Duration.convexitySimple(cashflows, discounter)
	      case Compounding.Continuous => Duration.convexityContinuous(cashflows, discounter)
	      case Compounding.Compounded | Compounding.SimpleThenCompounded => Duration.convexityCompounded(cashflows, discounter, freq.toInteger, getYield(comp, freq).getOrElse(Double.NaN))
	      case _ => None
	    }
	}
    
	
    def effectiveConvexity(shift:Double):Option[Double] = {
      val durationlow = rateDelta(-shift)
      val durationhigh = rateDelta(shift)
      (durationlow, durationhigh) match {
        case (Some(l), Some(h)) => Some((l + h) / shift)
        case _ => None
      }
    }
	
	def convexity:Option[Double] = effectiveConvexity(0.0001)
	
    /*
     * Remaining life in number of years
     */
	def remainingLife:Option[Double] = valueDate.collect{ case d => Date.daycount(d, terminationDate, new Actual365Fixed).max(0.0)}
	
	
	override def toString:String = id
	
	private def disp(name:String, f: => Any) = println(name + (" " * math.max(10 - name.size, 0)) + "\t" + (f match {
	  case null => "null"
	  case s:Option[Any] => s.getOrElse("None")
	  case s => s.toString
	}))
	
	override def getPriceHistory = DB.getHistorical("BONDJPY:" + id)
	
	override def latestPrice:Option[Double] = dirtyPrice
	
	override def expectedYield:Option[Double] = yieldContinuous
  
    override def expectedCoupon:Option[Double] = currentRate
	
    protected def getDbForwardPrice = DB.getForwardPrices("BOND", id)
	
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
} 


object Bond {
  
	def apply(db:dbBond):Option[Bond] = {
	  val tbdfixings = try { Some(db.settingMap("tbd").toDouble)} catch {case _:Throwable => None}
	  apply(db, tbdfixings)
	}
	
	def apply(db:dbBond, tbdfixing:Option[Double]):Option[Bond] = {
	  
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
	  
	  val scheduledPayoffs:ScheduledPayoffs = ScheduledPayoffs.sorted(schedule, coupon :+ redemption, calls.fill(schedule.size))
	  if (scheduledPayoffs == null || scheduledPayoffs.isEmpty) {println(db.id + ": cannot initialize scheduled payoffs"); return None}
		
	  Some(Bond(db, scheduledPayoffs, underlyings))
	}
  
}

