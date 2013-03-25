package squantlib.model

import org.jquantlib.currencies.Currency
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod, TimeUnit, Schedule => qlSchedule, _}
import org.jquantlib.termstructures.Compounding
import org.jquantlib.daycounters.{Absolute, Actual365Fixed, Thirty360, DayCounter}
import squantlib.database.schemadefinitions.{Bond => dbBond, BondPrice, Coupon => dbCoupon, ForwardPrice}
import squantlib.payoff._
import squantlib.model.rates.DiscountCurve
import squantlib.setting.initializer.{DayAdjustments, Currencies, Daycounters}
import squantlib.util.JsonUtils._
import squantlib.database.fixings.Fixings
import squantlib.pricing.model.{PricingModel, NoModel}
import squantlib.math.solver._
import squantlib.math.financial.{BondYield, Duration}
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.node.{JsonNodeFactory, ObjectNode, ArrayNode}
import org.codehaus.jackson.map.ObjectMapper
import org.codehaus.jackson.`type`.TypeReference
import scala.collection.JavaConversions._
import scala.collection.mutable.{Set => mutableSet}
import scala.collection.mutable.WeakHashMap
import java.math.{MathContext => JMC, RoundingMode}
import java.util.{Map => JavaMap}
import scala.collection.LinearSeq

/**
 * Bond class with enclosed risk analysis functions.
 */
case class Bond(
		db:dbBond, 
		inputSchedule:Schedule,
		inputCoupon:Payoffs,	
		redemption:Payoff,
		underlyings:List[String],
		callabilities:Callabilities,
		var defaultModel:(Market, Bond) => Option[PricingModel] = null,
		var forceModel:Boolean = false,
		var useCouponAsYield:Boolean = false,
		var requiresCalibration:Boolean = false,
		var modelCalibrated:Boolean = false,
		var _market:Option[Market] = None,
		var model:Option[PricingModel] = None
		) {
	
	/*	
	 * Returns full bond schedule
	 */
	val scheduledPayoffs:ScheduledPayoffs = {
	  ScheduledPayoffs.sorted(inputSchedule, inputCoupon :+ redemption, callabilities.fill(inputSchedule.size))
	}
	
	val schedule = scheduledPayoffs.schedule
	
	val payoffs = scheduledPayoffs.payoffs
	
	val calls = scheduledPayoffs.calls
	
	val coupon = scheduledPayoffs.coupon
  
	/*
	 * Standard bond parameters
	 */
	val id = db.id
	
	val issueDate:qlDate = schedule.head.startDate
	
	val maturity:qlDate = schedule.last.endDate
	
	val bermudan = calls.bermudans
	
	val trigger = calls.triggers
	
	val nominal:Option[Double] = db.nominal
	
	val currency:Currency = Currencies(db.currencyid).orNull
	
	val denomination:Option[Double] = db.denomination
	
	val period:qlPeriod = (db.coupon_freq collect { case f => new qlPeriod(f, TimeUnit.Months)}).orNull

	val calendar:Calendar = db.calendar
	
	val issuePrice:Option[Double] = db.issueprice
	
	val call:String = db.call
	
	val initialFX:Double = db.initialfx
	
	var issuer:String = db.issuerid
	
	val settings:JsonNode = db.settings.jsonNode.getOrElse((new ObjectMapper).createObjectNode)
	
	val isFixedRateBond = payoffs.variables.size == 0
	
	def isMatured:Option[Boolean] = valueDate.collect { case vd => vd ge maturity}
	
	override def clone:Bond = {
	  val bond = new Bond(db, schedule, coupon, redemption, underlyings, calls, defaultModel, forceModel, useCouponAsYield, requiresCalibration, modelCalibrated, _market, model) 
	  calibrationCache.foreach{case (a, b) => bond.calibrationCache.update(a, b)}
	  bond
	}
	
	def dateShifted(shift:Int):Bond = 
	  new Bond(db, schedule.shifted(shift), coupon, redemption, underlyings, calls, defaultModel, forceModel, useCouponAsYield, requiresCalibration, modelCalibrated, _market, model)

	def triggerShifted(trig:List[List[Option[Double]]]):Bond = 
	  new Bond(db, schedule, coupon, redemption, underlyings, Callabilities(bermudan, trig, underlyings), defaultModel, forceModel, useCouponAsYield, requiresCalibration, modelCalibrated, _market, model)
	
	
	def market:Option[Market] = _market
	def market_= (newMarket:Market) = {
	  val recalib = market match {
	    case Some(m) => !m.valuedate.eq(newMarket.valuedate) 
	    case None => true}
	  _market = Some(newMarket)
	  initializeModel(recalib)
	}
	
	def setMarket(newMarket:Market):Unit = market = newMarket
	
	
	/* 
	 * Reset model
	 */
	def initializeModel(reCalibrate:Boolean = false):Unit = {
	  if (reCalibrate) {calibrationCache.clear; modelCalibrated = false}
	  
	  model = market match {
	    case (Some(mkt)) if mkt.valuedate lt maturity => livePayoffs match {
	    	case po if !po.isEmpty && !forceModel && po.payoffs.variables.size == 0 => Some(NoModel(po.payoffs, po.schedule))
	    	case _ => if (defaultModel == null) None else defaultModel(mkt, this)
	    }
	    case _ => None
	  }
	  cpncache.clear
	  if (requiresCalibration && !modelCalibrated) {
	    modelCalibrated = true
	    calibrateModel
	  }
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
	  case Some(m) => {model = Some(m.calibrate); cpncache.clear}
	  case None => {}
	}
	
	val calibrationCache = new WeakHashMap[String, Any]
	def getCalibrationCache[A](k:String):Option[A] = 
	  if (calibrationCache contains k) calibrationCache(k) match {
	    case obj:A => Some(obj)
	    case _ => None}
	  else None
	
	def valueDate:Option[qlDate] = market.collect{case mkt => mkt.valuedate}
	
	
	/*	
	 * Returns "live" schedules
	 * 	@returns Schedule containing legs with payment date after market value date or specified value date.
	 */
	def liveSchedule:Schedule = valueDate.collect{case d => liveSchedule(d)}.orNull
	
	def liveSchedule(vd:qlDate):Schedule = Schedule(schedule.toList.filter(_.paymentDate gt vd))
	
	
	/*	
	 * Returns "live" payment schedules
	 * 	@returns element 1: Schedule containing legs with payment date after market value date or specified value date.
	 * 			element 2: Payoffs containing legs with payment dates after market value date or specified value date.
	 */
	def livePayoffs:ScheduledPayoffs = valueDate.collect {case d => livePayoffs(d)}.getOrElse(ScheduledPayoffs.empty)

	def livePayoffs(vd:qlDate):ScheduledPayoffs = getFixedPayoffs(scheduledPayoffs.withValueDate(vd), Some(vd))
		
	def allPayoffs:ScheduledPayoffs = getFixedPayoffs(scheduledPayoffs)
	
	def getFixedPayoffs(payoffSchedule:ScheduledPayoffs, vd:Option[qlDate] = None):ScheduledPayoffs = ScheduledPayoffs(
	    payoffSchedule.map{
	      case (period, payoff, call) if payoff.variables.size == 0 => (period, payoff, call)
	      case (period, payoff, call) if (vd.isDefined && (period.eventDate gt vd.get)) => (period, payoff, call)
	      case (period, payoff, call) => {
	        val fixings = payoff.variables.map(v => Fixings.byDate(v, period.eventDate).collect{case (d, f) => (v, f)}).flatMap(x => x).toMap
    	    (period, payoff.applyFixing(fixings), call)
    	  }
    	}, vd)
	
	/*	
	 * Returns "live" triggers
	 * 	@returns list of remaining triggers
	 */
	def liveTriggers:List[List[Option[Double]]] = livePayoffs.calls.triggers
	
	def liveTriggers(vd:qlDate):List[List[Option[Double]]] = livePayoffs(vd).calls.triggers
	
	/*	
	 * Returns "live" bermudan call options
	 * 	@returns list of remaining bermudan calls
	 */
	def liveBermudans:List[Boolean] = livePayoffs.calls.bermudans
		
	def liveBermudans(vd:qlDate):List[Boolean] = livePayoffs(vd).calls.bermudans
	
	/*	
	 * Returns discount curve.
	 * 	@returns discount curve created from either pre-set or specified market
	 */
	def discountCurve:Option[DiscountCurve] = market.flatMap(m => m.getDiscountCurve(currency, issuer))
	
	/*	
	 * Returns discount curve.
	 * 	@returns discount curve created from either pre-set or specified market
	 */
	def discountFactors:Option[List[(qlDate, Double)]] = (discountCurve, valueDate) match {
	  case (Some(curve), Some(vd)) => Some(schedule.paymentDates.withFilter(_ gt vd).map(d => (d, curve(d))))
	  case _ => None
	}

	/*
	 * Temporal cache to store spot and forward coupons.
	 */
	val cpncache = new scala.collection.mutable.WeakHashMap[String, List[(CalculationPeriod, Double)]]
	
	
	/*	
	 * Returns coupons fixed with current spot market (not forward!). 
	 */
	def spotFixedRates:List[(CalculationPeriod, Double)] = cpncache.getOrElseUpdate("SPOTFIXEDRATES",
	    livePayoffs.toList.map{case (d, p, _) => (d, market match { case Some(mkt) => p.price(mkt) case None => Double.NaN})}
	  )
	def spotFixedRates(vd:qlDate):List[(CalculationPeriod, Double)] = spotFixedRates.filter{case (p, d) => (p.paymentDate gt vd)}
	  
	def spotFixedAmount:List[(qlDate, Double)] = spotFixedRates.map{case (period, rate) => (period.paymentDate, rate * period.dayCount)}
	
	def spotFixedAmount(vd:qlDate):List[(qlDate, Double)] = spotFixedAmount.filter{case (d, _) => (d gt vd)}
	  
	def spotFixedRatesAll:List[(CalculationPeriod, Double)] = cpncache.getOrElseUpdate("SPOTFIXEDRATESALL",
	    allPayoffs.toList.map{case (d, p, _) => (d, market match { case Some(mkt) => p.price(mkt) case None => Double.NaN})}
	  )
	def spotFixedAmountAll:List[(qlDate, Double)] = spotFixedRatesAll.map{case (period, rate) => (period.paymentDate, rate * period.dayCount)}
	
    def spotCashflowDayfrac(dc:DayCounter):List[(Double, Double)] = spotFixedAmount.map{
      case (payday, amount) => (dc.yearFraction(valueDate.get, payday), amount)}
	
	/*	
	 * Returns forward value of each coupon (not discounted)
	 */
	def forwardLegs:Option[List[(CalculationPeriod, Double)]] = if (cpncache.contains("FORWARDLEGS")) Some(cpncache("FORWARDLEGS"))
	 else {
	  val result = valueDate.flatMap { case d => 
	    model match {
	      case None => println(id + " : model calibration error"); None
	      case Some(mdl) => Some((livePayoffs(d).schedule zip mdl.priceLegs).toList)
	    }}
	  
	    result match {
	      case Some(r) if r.forall{case (_, p) => !p.isNaN} => cpncache("FORWARDLEGS") = r
	      case _ => {}
	    }
	    result
	 }
	    
	
	/*	
	 * Returns price legs of the bond. (including accrued interest)
	 */
	def priceLegs:Option[List[Double]] = (discountCurve, forwardLegs) match {
	  case (Some(curve), Some(fwd)) if !fwd.isEmpty => Some(fwd.map{ case (d, p) => d.coefficient(curve) * p})
	  case _ => None
	}
	
	def europeanPrice:Option[Double] = model.flatMap(m => {
	  val price = if (m.isPricedByLegs) priceLegs.collect{case legs => legs.sum} else m.discountedPrice(discountCurve)
	  price match {case Some(p) if !p.isNaN => price case _ => None}
	})
	  
	def optionPrice:Option[Double] = model.flatMap{case m => m.optionValue} 
	
	/*	
	 * Returns dirty price of the bond. (ie. including accrued interest)
	 */
	def dirtyPrice:Option[Double] = europeanPrice.collect{
	  case p => p + optionPrice.getOrElse(0.0)
	}
	  
	/*	
	 * Returns clean price of the bond (ie. Dirty price - accrued coupon)
	 */
	def cleanPrice:Option[Double] = (dirtyPrice, accruedAmount) match { 
	  case (Some(d), Some(a)) => Some(d - a) case _ => None}
	
	/*	
	 * Returns accrued coupon.
	 */
	def accruedAmount:Option[Double] = market.flatMap(mkt => 
	  if (issueDate ge mkt.valuedate) Some(0.0)
	  else livePayoffs.filter{case (d, p, _) => (d.isCurrentPeriod(mkt.valuedate) && !d.isAbsolute)} match {
	    case pos if pos.isEmpty => None
	    case pos => Some(pos.map{case (d, p, _) => (d.accrued(mkt.valuedate)) * p.price(mkt) }.sum)
	  })
	  
	/*	
	 * Returns model price of the bond, different from dirty price if there's officially published price different from model valuation.
	 */
	def modelPrice:Option[Double] = europeanPrice.collect{
	  case p => p + optionPrice.getOrElse(0.0)
	}

	/*	
	 * Returns current coupon rate.
	 */
//	def currentRate:Option[Double] = market collect { case mkt => scheduledPayoffs.withFilter{case (d, _, _) => (d.isCurrentPeriod(mkt.valuedate) && !d.isAbsolute)}
//	  								.map{case (d, p, _) => p.price(mkt) }.sum}
	def currentRate:Option[Double] = market.flatMap{case mkt => 
	  scheduledPayoffs.filter{case (d, _, _) => ((d.paymentDate gt mkt.valuedate) && !d.isAbsolute)} match {
	    case ds if ds.isEmpty => None
	    case ds => ds.minBy{case (d, _, _) => d.paymentDate} match {case (d, p, _) => Some(p.price(mkt))}}
	}

	/*	
	 * Returns next coupon payment date
	 */
	def nextPayment:Option[(qlDate, Double)] = market.flatMap{case mkt => 
	  scheduledPayoffs.filter{case (d, _, _) => ((d.paymentDate gt mkt.valuedate) && !d.isAbsolute)} match {
	    case ds if ds.isEmpty => None
	    case ds => ds.minBy{case (d, p, _) => d.paymentDate} match {case (d, p, _) => Some(d.paymentDate, d.dayCount * p.price(mkt))}}
	}
	
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
        accruedAmount.collect{case acc => (cashflows.unzip._2.sum - acc - 1.0) / cashflows.unzip._1.max}
      } else dirtyPrice.flatMap{case p => getYield(p, dc, comp, freq, accuracy, maxIteration)}
      
      if (result == Some(Double.NaN)) None else result
	}
	
    def getYield(price:Double, dc:DayCounter, comp:Compounding, freq:Frequency, accuracy:Double, maxIteration:Int):Option[Double] = 
      valueDate.flatMap{ case vd => getYield(price, spotCashflowDayfrac(dc), comp, freq, accuracy, maxIteration, vd)}
	
    def getYield(price:Double, dc:DayCounter, comp:Compounding, freq:Frequency, accuracy:Double, maxIteration:Int, vd:qlDate):Option[Double] = 
      getYield(price, spotCashflowDayfrac(dc), comp, freq, accuracy, maxIteration, vd)
      
    def getYield(price:Double, cashflows:List[(Double, Double)], comp:Compounding, freq:Frequency, accuracy:Double, maxIteration:Int, vd:qlDate):Option[Double] = 
      if (cashflows isEmpty) None
      else if (useCouponAsYield) accruedAmount.collect{case acc => BondYield.asAverageCoupon(cashflows, acc)}
	  else comp match {
        case Compounding.Simple => BondYield.solveNoCompounding(price, cashflows, accuracy, maxIteration)
	    case Compounding.Compounded | Compounding.SimpleThenCompounded => BondYield.solveCompounded(price, cashflows, freq.toInteger, accuracy, maxIteration)
	    case Compounding.Continuous => BondYield.solveContinuous(price, cashflows, accuracy, maxIteration)
	    case Compounding.None => accruedAmount.collect{case acc => BondYield.solveNoRate(price, cashflows, acc)}
	    case _ => None
      }
    
	/*	Returns each bond yield.
	 */
	def yieldNoCompounding:Option[Double] = getYield(Compounding.Simple)
	
	def yieldContinuous:Option[Double] = getYield(Compounding.Continuous)
	
	def yieldSemiannual:Option[Double] = getYield(Compounding.Compounded, Frequency.Semiannual)
	
	def yieldAnnual:Option[Double] = getYield(Compounding.Compounded, Frequency.Annual)
	
	def yieldSimple:Option[Double] = getYield(Compounding.None, Frequency.Annual)
	
	/*	
	 * Returns next bermudan callable date.
	 */
	def nextBermudan:Option[qlDate] = valueDate.flatMap{case vd => 
	  (schedule zip bermudan).filter{case (d, b) => (b && (d.paymentDate gt vd) && !d.isAbsolute)} match {
	    case ds if ds.isEmpty => None
	    case ds => Some(ds.map{case (d, b) => d.paymentDate}.min)
	}}
	
	/*	Returns yield at which bond price becomes 100% (if any)
	 * @param comp Compounding rate, as one of the following
	 * 		"None" => Not applicable
	 * 		"Simple" => No compounding : ZC = 1 / rt
	 * 		"Compounded" => Standard compounding: ZC = 1 / (1+r/f)^tf 	
	 * 		"Continuous" => 
	 */
	def parMtMYield:Option[Double] = getYield(1.0, new Actual365Fixed, Compounding.Continuous, null, 0.00001, 20)
	
	/*	
	 * Continuous rate at which MtM exceeds 100% at next call date.
	 */
	def nextRateFrontier:Option[Double] = nextBermudan.flatMap{ case d => getYield(1.0, new Actual365Fixed, Compounding.Continuous, null, 0.00001, 20, d) }
	
	/*	
	 * Returns FX at which JPY dirty bond price becomes 100% (if any)
	 */
	def parMtMfx:Option[Double] = if (currency.code == "JPY") None else dirtyPrice.collect{case p => db.initialfx / p }
	
	/*	
	 * Returns FX at which JPY dirty bond price becomes 100% at any given date
	 */
	def fxFrontier:List[Option[Double]] = nextBermudan match {
	  case Some(d) => fxFrontier(1.00, 0.001, 20, d)
	  case None => List.fill(underlyings.size)(None)
	}
	
	def fxFrontier(vd:qlDate):List[Option[Double]] = fxFrontier(1.00, 0.001, 20, vd)

    def fxFrontier(target:Double, 
        accuracy:Double, 
        maxIteration:Int, 
        vd:qlDate, 
        paths:Int = 0, 
        solver:RangedRootFinder = Bisection, 
        highRange:Double = 10.0, 
        lowRange:Double = 0.001):List[Option[Double]] = 
      
      if (market.isEmpty) List.fill(underlyings.size)(None)
      
      else {
        val mkt = market.get
        val shift = this.valueDate.get.sub(vd).toInt
//        println("current bond")
//        this.show
//        println("create date shifted clone => shift by " + shift + " to " + vd)
        val bond = this.dateShifted(this.valueDate.get.sub(vd).toInt)
//        println("clone done : " + bond.livePayoffs.size + " legs")
//        bond.show
        
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
      
      val valuedates = (liveSchedule zip liveBermudans)
    		  	.zipWithIndex
    		  	.filter{case ((_, t), _) => t == true}
      			.map{case ((d, _), index) => (d.paymentDate, index)}
      			.toList
      			.sortBy{case (date, _) => date}
      			.reverse
      
      val tempTrigger = scala.collection.mutable.ArrayBuffer(liveTriggers:_*)
      println("FX frontiers : " + id)
      
      valuedates.foreach{case (vd, index) => 
        val tempBond = triggerShifted(tempTrigger.toList)
        if (liveTriggers(index).isEmpty) {
          tempTrigger(index) = tempBond.fxFrontier(1.00, accuracy, maxIteration, vd, paths)
          println(index + " : " + tempTrigger(index).mkString(","))
        }
      }
      tempTrigger.toList
    }
	
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
    def macaulayDuration:Option[Double] = discountCurve.flatMap{case curve => 
      val mac = Duration.macaulay(spotCashflowDayfrac(new Actual365Fixed), (d:Double) => curve(d * 365.25))
      if (mac.isNaN) None else Some(mac)
    }
    
	/*	
	 * Returns modified duration defined as Macauley duration / (1 + yield / freq)
	 */
	def modifiedDuration:Option[Double] = modifiedDuration(Compounding.Compounded, Frequency.Annual)
	def modifiedDuration(comp:Compounding, freq:Frequency):Option[Double] = macaulayDuration.flatMap { case dur =>
	    comp match {
	      case Compounding.Continuous => Some(dur)
	      case Compounding.Compounded | Compounding.SimpleThenCompounded => getYield(comp, freq).collect{case y => dur / (1.0 + y / freq.toInteger.toDouble)}
	      case Compounding.Simple => None
	  }
	}
	
	/*	
	 * Returns effective duration defined as 1bp rate delta * 10000
	 */
	def effectiveDuration:Option[Double] = rateDelta(-0.0001).collect{case d => d * 10000} // TO BE COMPUTED AS RATE DELTA
	
	/*	
	 * List of underlying currencies
	 */
	def currencyList:Set[String] = {
	  var result = scala.collection.mutable.Set.empty[String]
	  livePayoffs.payoffs.variables.foreach {
	    case c if c.size == 6 => if (Currencies contains (c take 3)) result += (c take 3)
			  				  if (Currencies contains (c takeRight 3)) result += (c takeRight 3)
	    case c if c.size >= 3 => if (Currencies contains (c take 3)) result += (c take 3)
	    case _ => {}
	  }
	  (result + currency.code).toSet
	}

	def greek(target:Bond => Option[Double], operation:Market => Market) = market.flatMap { case mkt =>
	  val initprice = target(this)
	  val newBond = this.clone
	  newBond.market = operation(mkt)
	  val newprice = target(newBond)
	  (initprice, newprice) match { case (Some(i), Some(n)) => Some(n - i) case _ => None }
	}
	
	/*	
	 * Returns rate delta
	 */
	def rateDelta(shift:Double):Option[Double] = rateDelta(currency.code, shift)
	
	def rateDelta(ccy:String, shift:Double):Option[Double] = rateDelta((b:Bond) => b.dirtyPrice, Map(ccy -> shift))
	
	def rateDelta(target:Bond => Option[Double], shift:Map[String, Double]):Option[Double] = greek(target, (m:Market) => m.rateShifted(shift))

	
	/*	
	 * Returns rate delta for all involved currencies.
	 */
	def rateDeltas(shift:Double):Map[String, Double] = currencyList.map(f => (f, rateDelta(f, shift))).collect{case (a, Some(b)) => (a, b)}.toMap

	
	/*	
	 * Returns FX delta on JPY bond price.
	 */
	def fxDeltaJpy(mult:Double):Map[String, Double] = (currencyList - "JPY").map(f => 
	  (f + "JPY", fxDelta((b:Bond) => b.dirtyPriceJpy, Map(f -> 1/mult)))).collect{case (a, Some(b)) => (a, b)}.toMap

	  
	/*	
	 * Returns delta of 1 yen change in FX on JPY price.
	 */
	def fxDeltaOneJpy:Map[String, Double] = market match {
	  case None => Map.empty
	  case Some(mkt) => (currencyList - "JPY").map(f => mkt.fx(f, "JPY") match {
	      case Some(fx) => (f + "JPY", fxDelta((b:Bond) => b.dirtyPriceJpy, Map(f -> fx/(fx+1))))
	      case None => (f + "JPY", None)
	    }).collect{case (a, Some(b)) => (a, b)}.toMap}
	    
	/*	
	 * Returns FX delta for all involved currencies.
	 */
	def fxDeltas(mult:Double):Map[String, Double] = (currencyList - currency.code).map(ccy => ("USD" + ccy, fxDelta((b:Bond) => b.dirtyPrice, Map(ccy -> mult)))).collect{case (a, Some(b)) => (a, b)}.toMap
	
	def fxDelta(ccy:String, mult:Double):Option[Double] = fxDelta((b:Bond) => b.dirtyPrice, Map(ccy -> mult))
	
	def fxDelta(target:Bond => Option[Double], mult:Map[String, Double]):Option[Double]	= greek(target, (m:Market) => m.fxShifted(mult))
	
	
	/*	
	 * List of FX underlyings
	 */
	def fxList:Set[String] = livePayoffs.payoffs.variables.filter(
	  c => ((c.size == 6) && (Currencies contains (c take 3)) && (Currencies contains (c takeRight 3))))

	/*	
	 * Returns rate vega
	 */
	def fxVegas(addvol:Double):Map[String, Double] = fxList.map(fx => (fx, fxVega(fx, addvol))).collect{case (a, Some(b)) => (a, b)}.toMap
	
	def fxVega(ccypair:String, addvol:Double):Option[Double] = fxVega((b:Bond) => b.dirtyPrice, Map(ccypair -> addvol))
	
	def fxVega(target:Bond => Option[Double], addvol:Map[String, Double]):Option[Double] = greek(target, (m:Market) => m.fxVolShifted(addvol))
	  
	
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
	def remainingLife:Option[Double] = valueDate.collect{ case d => (new Actual365Fixed).yearFraction(d, maturity)}
	
	
    /*
     * Output to BondPrice object
     */
	def mapToJsonString(params:Map[String, Double]):String = {
	  val javamap:java.util.Map[String, Double] = params
	  (new ObjectMapper).writeValueAsString(javamap)
	}
	
	def toBondPrice:Option[BondPrice] = (market, cleanPrice) match {
	  case (Some(mkt), Some(p)) => Some(new BondPrice(
	  		id = id + ":" + mkt.paramset + ":" + currency.code,
			bondid = id,
			currencyid = currency.code,
			comment = null,
			paramset = mkt.paramset,
			paramdate = mkt.valuedate.longDate,
			fxjpy = fxjpy.getOrElse(0),
			pricedirty = dirtyPrice.collect{case p => p * 100}.getOrElse(Double.NaN),
			priceclean = cleanPrice.collect{case p => p * 100},
			accrued = accruedAmount.collect{case p => p * 100},
			pricedirty_jpy = dirtyPriceJpy.collect{case p => p * 100},
			priceclean_jpy = cleanPriceJpy.collect{case p => p * 100},
			accrued_jpy = accruedAmountJpy.collect{case p => p * 100},
			yield_continuous = yieldContinuous,
			yield_annual = yieldAnnual,
			yield_semiannual = yieldSemiannual,
			yield_simple = yieldSimple,
			bpvalue = bpvalue.collect{case p => p * 100},
			irr = irr,
			currentrate = currentRate,
			nextamount = nextPayment.collect{case (d, p) => p * 100},
			nextdate = nextPayment.collect{case (d, p) => d.longDate},
			dur_simple = effectiveDuration,
			dur_modified = modifiedDuration,
			dur_macauley = macaulayDuration,
			yieldvaluebp = yieldValueBasisPoint,
			convexity = convexity,
			remaininglife = remainingLife,
			parMtMYield = parMtMYield,
			parMtMfx = parMtMfx,
			rateDelta = mapToJsonString(rateDeltas(0.001)),
			rateVega = null,
			fxDelta = mapToJsonString(fxDeltas(1.01)),
			fxDeltaJpy = mapToJsonString(fxDeltaOneJpy),
			fxVega = mapToJsonString(fxVegas(0.01)),
			created = Some(new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime)),
			lastmodified = Some(new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime))))
	  
	  case _ => None
	} 

	def toQuickBondPrice:Option[BondPrice] = (market, cleanPrice) match {
	  case (Some(mkt), Some(p)) => Some(new BondPrice(
	  		id = id + ":" + mkt.paramset + ":" + currency.code,
			bondid = id,
			currencyid = currency.code,
			comment = null,
			paramset = mkt.paramset,
			paramdate = mkt.valuedate.longDate,
			fxjpy = fxjpy.getOrElse(0),
			pricedirty = dirtyPrice.collect{case p => p * 100}.getOrElse(Double.NaN),
			priceclean = cleanPrice.collect{case p => p * 100},
			accrued = accruedAmount.collect{case p => p * 100},
			pricedirty_jpy = dirtyPriceJpy.collect{case p => p * 100},
			priceclean_jpy = cleanPriceJpy.collect{case p => p * 100},
			accrued_jpy = accruedAmountJpy.collect{case p => p * 100},
			yield_continuous = None,
			yield_annual = None,
			yield_semiannual = None,
			yield_simple = None,
			bpvalue = None,
			irr = None,
			currentrate = currentRate,
			nextamount = nextPayment.collect{case (d, p) => p * 100},
			nextdate = nextPayment.collect{case (d, p) => d.longDate},
			dur_simple = None,
			dur_modified = None,
			dur_macauley = None,
			yieldvaluebp = None,
			convexity = None,
			remaininglife = remainingLife,
			parMtMYield = None,
			parMtMfx = None,
			rateDelta = null,
			rateVega = null,
			fxDelta = null,
			fxDeltaJpy = null,
			fxVega = null,
			created = Some(new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime)),
			lastmodified = Some(new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime))))
	  
	  case _ => None
	} 
	
	val defaultMathContext = new JMC(34, RoundingMode.HALF_UP)
	
	def toCoupons:Set[dbCoupon] = {
	  val pos = allPayoffs
	  val spot:List[Double] = spotFixedRatesAll.unzip._2
	  
	  (0 to pos.size - 1).map(i => {
	      val (s, p, t) = pos(i)
	      val fixedrate = p match {case po:FixedPayoff if !po.payoff.isNaN => Some(po.payoff) case _ => None }
	      val fixedamount = fixedrate.collect{case r => r * s.dayCount}
	      val paytype = if (s isAbsolute) "REDEMPTION" else "COUPON"
	      val spotrate = spot(i) match {case v if v.isNaN || fixedrate.isDefined => None case _ => Some(spot(i)) }
	      val spotamount = spotrate.collect{case r => r * s.dayCount}
	      
	      new dbCoupon(
	          id = id + ":" + i + ":" + paytype,
	          bondid = id,
	          currency = currency.code,
	          rate = payoffs(i).toString,
	          eventdate = s.eventDate.longDate,
	          startdate = s.startDate.longDate,
	          enddate = s.endDate.longDate,
	          paymentdate = s.paymentDate.longDate,
	          fixedrate = fixedrate.collect{case v => BigDecimal(v, defaultMathContext)},
	          fixedamount = fixedamount.collect{case v => BigDecimal(v, defaultMathContext)},
		      spotrate = spotrate.collect{case v => BigDecimal(v, defaultMathContext)},
		      spotamount = spotamount.collect{case v => BigDecimal(v, defaultMathContext)},
	          jsonformat = payoffs(i).jsonString,
	          display = p.display(paytype == "REDEMPTION"),
	          comment = p.description,
	          daycount = s.daycounter.toString,
	          paymenttype = paytype,
			  lastmodified = Some(new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime))
			  )}).toSet
	}
	
	
	def toForwardPrice(vd:qlDate, fwdfx:Double):Option[ForwardPrice] = (market, cleanPrice) match {
	  case (Some(mkt), Some(cp)) => Some(new ForwardPrice(
	      id = "BOND:" + id + ":" + mkt.paramset + ":" + ("%tY%<tm%<td" format mkt.valuedate.longDate),
	      paramset = mkt.paramset,
	      paramdate = vd.longDate,
	      valuedate = mkt.valuedate.longDate,
	      underlying = "BOND:" + id,
	      value = cp,
	      valuejpy = if (db.initialfx == 0 || fwdfx == 0) None else Some(cp * fwdfx / db.initialfx),
	      created = Some(new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime))
	      ))
	  case _ => None
	}
	  
	override def toString:String = id
	
	private def disp(name:String, f: => Any) = println(name + (" " * math.max(10 - name.size, 0)) + "\t" + (f match {
	  case null => "null"
	  case s:Option[Any] => s.getOrElse("None")
	  case s => s.toString
	}))
	
	def show:Unit = {
	    disp("id", id)
	    disp("currency", currency.code)
	    disp("model", model match { case None => "Not defined" case Some(m) => m.getClass.getName})
	    disp("market", market match { case None => "Not defined" case Some(m) => m.paramset})
	    disp("underlyings", underlyings.mkString(" "))
	    
	    if (market isDefined) {
	      println("Live payoffs:") 
//	      livePayoffs.foreach{case (s, po, _) => disp(s.toString, po)}
//	      disp("triggers", liveTriggers.mkString(","))
//	      disp("bermudans", liveBermudans.mkString(","))
	      println(livePayoffs.toString)
	    }
	    else {
	      println("Full schedule:")
		  scheduledPayoffs.foreach{case (s, po, _) => disp(s.toString, po)}
	      disp("triggers", trigger.mkString(","))
	      disp("bermudans", bermudan.mkString(","))
	    }
	  }
	
	def showAll:Unit = {
	  show
	  println("Price Details:") 
	  toBondPrice match {
	    case Some(p) => p.getFieldMap.toList.sortBy(_._1).foreach{case (k, v) => disp(k, v)}
	    case None => {}
	  }
	}
	
	/*	Returns message returned by pricing model.
	 */
	def modelmsg:Unit = model match { case None => {} case Some(m) => m.message.foreach(println) }
	
} 


object Bond {
  
	def apply(db:dbBond):Option[Bond] = {
	  
	  val schedule = db.schedule.orNull
	  if (schedule == null) {return None}
		
	  val fixings:Map[String, Double] = if (!db.fixingList.isEmpty) db.fixingList
			  else if (db.fixingdate.isDefined && db.fixingdate.get.after(Fixings.latestParamDate.longDate)) Fixings.latestList(db.underlyingList)
			  else Map.empty

	  val coupon:Payoffs = Payoffs(db.fixedCoupon(fixings), schedule.size - 1).orNull
		
	  val redemption = Payoff(db.fixedRedemprice(fixings)).orNull
		
	  val underlyings:List[String] = db.underlyingList
		
	  val bermudan = db.bermudanList(fixings, schedule.size)
		
	  val trigger = db.triggerList(fixings, schedule.size)
	  
	  val calls = Callabilities(bermudan, trigger, underlyings)
		
	  if (List(db, schedule, coupon, redemption, underlyings, bermudan, trigger).forall(_ != null) && (coupon.size + 1 == schedule.size))
		  Some(Bond(db, schedule, coupon, redemption, underlyings, calls))
	  else None
	  
	}
  
}
	
