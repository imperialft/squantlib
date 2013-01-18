package squantlib.model

import org.jquantlib.currencies.Currency
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod, TimeUnit, _}
import org.jquantlib.termstructures.Compounding
import org.jquantlib.daycounters.{Absolute, Actual365Fixed, Thirty360, DayCounter}
import squantlib.database.schemadefinitions.{Bond => dbBond, BondPrice, Coupon => dbCoupon, ForwardPrice}
import squantlib.payoff.{Payoffs, Schedule, Payoff, CalcPeriod, FixedPayoff}
import squantlib.model.rates.DiscountCurve
import squantlib.setting.initializer.{DayAdjustments, Currencies, Daycounters}
import squantlib.util.JsonUtils._
import squantlib.database.fixings.Fixings
import squantlib.pricing.model.{PricingModel, NoModel}
import squantlib.math.solver.NewtonRaphson
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.node.{JsonNodeFactory, ObjectNode, ArrayNode}
import org.codehaus.jackson.map.ObjectMapper
import org.codehaus.jackson.`type`.TypeReference;
import scala.collection.JavaConversions._
import scala.collection.mutable.{Set => mutableSet}
import java.math.{MathContext => JMC, RoundingMode}
import java.util.{Map => JavaMap}

/**
 * Bond class with enclosed risk analysis functions.
 */
case class Bond(
		db:dbBond, 
		inputSchedule:Schedule,
		coupon:Payoffs,	
		redemption:Payoff,
		underlyings:List[String],
		bermudan:List[Boolean],
		trigger:List[List[Option[Double]]]) {
	
	/*
	 * Standard bond parameters
	 */
	val id = db.id
	
	val issueDate:qlDate = inputSchedule.first.startDate
	
	val maturity:qlDate = inputSchedule.last.endDate
	
	val nominal:Double = db.nominal
	
	val currency:Currency = Currencies(db.currencyid).orNull
	
	val denomination:Option[Double] = db.denomination
	
	val period:qlPeriod = (db.coupon_freq collect { case f => new qlPeriod(f, TimeUnit.Months)}).orNull

	val calendar:Calendar = db.calendar
	
	val issuePrice:Option[Double] = db.issueprice
	
	val call:String = db.call
	
	val initialFX:Double = db.initialfx
	
	var issuer:String = db.issuerid
	
	val settings:JsonNode = db.settings.jsonNode.getOrElse((new ObjectMapper).createObjectNode)
	
	val isFixedRateBond = coupon.variables.size == 0
	
	def isMatured:Option[Boolean] = valueDate.collect { case vd => vd ge maturity}
	
	override def clone:Bond = Bond(db, inputSchedule, coupon, redemption, underlyings, bermudan, trigger)
	
	def dateShifted(shift:Int):Bond = Bond(db, inputSchedule.shifted(shift), coupon, redemption, underlyings, bermudan, trigger)

	def triggerShifted(trig:List[List[Option[Double]]]):Bond = Bond(db, inputSchedule, coupon, redemption, underlyings, bermudan, trig)
	
	/* 
	 * Specify default market parameters
	 */
	protected var _market:Option[Market] = None
	
	def market:Option[Market] = _market
	def market_= (newMarket:Market) = {
	  _market = Some(newMarket)
	  initializeModel
	}
	
	def setMarket(newMarket:Market):Unit = market = newMarket
	
	/* 
	 * Pricing model
	 */
	var model:Option[PricingModel] = None
	
	/* 
	 * Use ModelSetter to define pricing model.
	 */
	var defaultModel:(Market, Bond) => Option[PricingModel] = null
	
	/* 
	 * Prevent using NoModel for fixed rate coupon legs automatically.
	 */
	protected var _forceModel = false
	
	def forceModel:Boolean = _forceModel
	def forceModel_= (newParam:Boolean) = _forceModel = newParam
	
	
	/* 
	 * Use average of future coupons as yield
	 */
	var useCouponAsYield = false
	
	/* 
	 * Reset model
	 */
	def initializeModel = {
	  model = market match {
	    case (Some(mkt)) => livePayoffs match {
	    	case (dates, payoff) if !forceModel && payoff.variables.size == 0 => Some(NoModel(payoff, dates))
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
	  _market = Some(newMarket)
	  defaultModel = setter
	  initializeModel
	}
	
	/* 
	 * True if necessary to run calibrateModel to get accurate price.
	 */
	var requiresCalibration:Boolean = false
	var modelCalibrated:Boolean = false
	def calibrateModel = model match {
	  case Some(m) => {model = Some(m.calibrate); cpncache.clear}
	  case None => {}
	}
	
	val calibrationCache = new scala.collection.mutable.WeakHashMap[String, Any]
	def getCalibrationCache[A](k:String):Option[A] = 
	  if (calibrationCache contains k) calibrationCache(k) match {
	    case obj:A => Some(obj)
	    case _ => None}
	  else None
	
	def valueDate:Option[qlDate] = market.collect{case mkt => mkt.valuedate}
	
	/*
	 * Returns full bond schedule
	 * @returns full bond payment schedule (date only)
	 * @returns full bond payoff (no dates)
	 */
	val (schedule, payoffs):(Schedule, Payoffs) = inputSchedule.sortWith(coupon.toList :+ redemption) match { case (s, p) => (s, Payoffs(p.toList))}

	/*	
	 * Returns full bond schedule
	 * @returns list of calculation period & payoff
	 */
	val payoffLegs:List[(CalcPeriod, Payoff)] = (schedule.toList zip payoffs.toList).toList
	
	
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
	def livePayoffs:(Schedule, Payoffs) = valueDate.collect {case d => livePayoffs(d)}.getOrElse((Schedule.empty, Payoffs.empty))

	def livePayoffs(vd:qlDate):(Schedule, Payoffs) = getFixedPayoffs(payoffLegs.filter{case (cp, p) => (cp.paymentDate gt vd)}, vd)
	
	def allPayoffs:(Schedule, Payoffs) = getFixedPayoffs(payoffLegs)
	
	def allPayoffLegs:List[(CalcPeriod, Payoff)] = allPayoffs match {case (s, p) => (s.toList zip p.toList)}
	
	def getFixedPayoffs(payoffSchedule:List[(CalcPeriod, Payoff)], vd:qlDate = null):(Schedule, Payoffs) = {
	  val po = payoffSchedule.map{
    	case (_, payoff) if payoff.variables.size == 0 => payoff
    	case (period, payoff) if ((vd != null) && (period.eventDate gt vd)) => payoff
    	case (period, payoff) => {
    	  val fixings = payoff.variables.map(v => Fixings.byDate(v, period.eventDate).collect{
    	    case (d, f) => (v, f)}).flatMap(x => x).toMap
    	    payoff.applyFixing(fixings)
    	  }
    	}
	  (Schedule(payoffSchedule.unzip._1), Payoffs(po))
	}
	
	/*	
	 * Returns "live" triggers
	 * 	@returns list of remaining triggers
	 */
	def liveTriggers:List[List[Option[Double]]] = trigger takeRight liveSchedule.size
	def liveTriggers(vd:qlDate):List[List[Option[Double]]] = trigger takeRight liveSchedule(vd).size
	
	/*	
	 * Returns "live" bermudan call options
	 * 	@returns list of remaining bermudan calls
	 */
	def liveBermudans:List[Boolean] = bermudan takeRight liveSchedule.size
	def liveBermudans(vd:qlDate):List[Boolean] = bermudan takeRight liveSchedule(vd).size
	
	
	/*	
	 * Returns "live" payment schedules broken down into pairs of a Calculation Period and a Payoff
	 *  @param value date
	 * 	@returns element 1: Schedule containing legs with payment date after market or specified value date
	 * 			element 2: Payoffs containing legs with payment dates after market or specified value date
	 */
	def livePayoffLegs:List[(CalcPeriod, Payoff)] = livePayoffs match { case (s, p) => (s.toList zip p.toList)}
	def livePayoffLegs(vd:qlDate):List[(CalcPeriod, Payoff)] = livePayoffs(vd) match { case (s, p) => (s.toList zip p.toList)}
	
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
	private val cpncache = new scala.collection.mutable.WeakHashMap[String, List[(CalcPeriod, Double)]]
	
	
	/*	
	 * Returns coupons fixed with current spot market (not forward!). 
	 */
	def spotFixedRates:List[(CalcPeriod, Double)] = cpncache.getOrElseUpdate("SPOTFIXEDRATES",
	    livePayoffLegs.map{case (d, p) => (d, market match { case Some(mkt) => p.spotCoupon(mkt) case None => Double.NaN})}
	  )
	  
	def spotFixedRates(vd:qlDate):List[(CalcPeriod, Double)] = spotFixedRates.filter{case (p, d) => (p.paymentDate gt vd)}
	  
	def spotFixedAmount:List[(CalcPeriod, Double)] = cpncache.getOrElseUpdate("SPOTFIXEDAMOUNT",
	    spotFixedRates.map{case (d, p) => (d, p * d.dayCount)}
	  )
	  
	def spotFixedAmount(vd:qlDate):List[(CalcPeriod, Double)] = spotFixedAmount.filter{case (p, d) => (p.paymentDate gt vd)}
	  
	def spotFixedRatesAll:List[(CalcPeriod, Double)] = cpncache.getOrElseUpdate("SPOTFIXEDRATESALL",
	    allPayoffLegs.map{case (d, p) => (d, market match { case Some(mkt) => p.spotCoupon(mkt) case None => Double.NaN})}
	  )
	  
	def spotFixedAmountAll:List[(CalcPeriod, Double)] = cpncache.getOrElseUpdate("SPOTFIXEDAMOUNTALL",
	    spotFixedRatesAll.map{case (d, p) => (d, p * d.dayCount)}
	  )
	
	/*	
	 * Returns forward value of each coupon (not discounted)
	 */
	def forwardLegs:Option[List[(CalcPeriod, Double)]] = if (cpncache.contains("FORWARDLEGS")) Some(cpncache("FORWARDLEGS"))
	 else {
	  val result = valueDate.flatMap { case d => 
	    val (dates, payoff) = livePayoffs(d)
	    model match {
	      case None => println(id + " : model calibration error"); None
	      case Some(mdl) => Some((dates zip mdl.priceLegs).toList)
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
	
//	def modelPrice:Option[Double] = priceLegs.collect{case legs => legs.sum}
	
	def europeanPrice:Option[Double] = model.flatMap(m => 
	  if (m.isPricedByLegs) priceLegs.collect{case legs => legs.sum}
	  else m.discountedPrice(discountCurve)
	  )
	  
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
	def cleanPrice:Option[Double] = (dirtyPrice, accruedAmount) match { case (Some(d), Some(a)) => Some(d - a) case _ => None}
	
	/*	
	 * Returns accrued coupon.
	 */
	def accruedAmount:Option[Double] = market.flatMap(mkt => 
	  if (issueDate ge mkt.valuedate) Some(0.0)
	  else livePayoffLegs.filter{case (d, p) => (d.isCurrentPeriod(mkt.valuedate) && !d.isAbsolute)} match {
	    case pos if pos.isEmpty => None
	    case pos => Some(pos.map{case (d, p) => (d.accrued(mkt.valuedate)) * p.spotCoupon(mkt) }.sum)
	  })

	/*	
	 * Returns current coupon rate.
	 */
	def currentRate:Option[Double] = market collect { case mkt => payoffLegs.withFilter{case (d, p) => (d.isCurrentPeriod(mkt.valuedate) && !d.isAbsolute)}
	  								.map{case (d, p) => p.spotCoupon(mkt) }.sum}

	/*	
	 * Returns next coupon payment date
	 */
	def nextPayment:Option[(qlDate, Double)] = market.flatMap{case mkt => 
	  payoffLegs.filter{case (d, p) => ((d.paymentDate gt mkt.valuedate) && !d.isAbsolute)} match {
	    case ds if ds.isEmpty => None
	    case ds => ds.minBy{case (d, p) => d.paymentDate} match {case (d, p) => Some(d.paymentDate, d.dayCount * p.spotCoupon(mkt))}}
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
	 * Returns rate at which the MtM of the bond is target price.
	 */
	
    def getYield(target:Double, dc:DayCounter, comp:Compounding, freq:Frequency, accuracy:Double, maxIteration:Int):Option[Double] = 
      valueDate.flatMap{ case vd => getYield(target, dc, comp, freq, accuracy, maxIteration, vd) }
    
    def getYield(target:Double, dc:DayCounter, comp:Compounding, freq:Frequency, accuracy:Double, maxIteration:Int, vd:qlDate):Option[Double] = {
      if (comp == Compounding.None) return None
	  
	  val paylegs:List[(Double, Double)] = spotFixedRates.map{case (d, r) => (dc.yearFraction(vd, d.paymentDate), r * d.dayCount)}
	  if (paylegs.exists(_._2.isNaN)) return None
	    
	  def priceFromYield(y:Double):Double = {
	    def zc(d:Double) = comp match {
	      case Compounding.Simple => 1.0 / (1.0 + y * d)
	      case Compounding.Compounded | Compounding.SimpleThenCompounded => {val fr = freq.toInteger.toDouble; 1.0 / math.pow(1.0 + y / fr, fr * d)}
	      case Compounding.Continuous => math.exp(-y * d)
	      } 
	      paylegs.map{case (d, v) => v * zc(d)}.sum
	    }
	    
	    val priceformula = (y:Double) => (priceFromYield(y) - target)
	    NewtonRaphson.solve(priceformula, 0.03, accuracy, 0.01, maxIteration)
	  }

    
	/*	
	 * Returns bond yield.
	 * @param comp Compounding rate, as one of the following
	 * 		"None" => Discounting is not taken into account : ZC = 1.0
	 * 		"Simple" => No compounding : ZC = 1 / rt
	 * 		"Compounded" => Standard compounding: ZC = 1 / (1+r/f)^tf 
	 * 		"Continuous" => 
	 */
	def bondYield(comp:Compounding, freq:Frequency = Frequency.Annual):Option[Double] = bondYield(new Actual365Fixed, comp, freq, 0.00001, 20)
	
    def bondYield(dc:DayCounter, comp:Compounding, freq:Frequency, accuracy:Double, maxIteration:Int):Option[Double] = {
	  if (useCouponAsYield) (accruedAmount, valueDate) match { 
	    case (Some(amt), Some(vd)) => Some((spotFixedAmount.map(_._2).sum - amt - 1.0) / dc.yearFraction(vd, maturity))
	    case _ => None
	  	}
	  else if (comp == Compounding.None) 
      	market.flatMap {case mkt => 
      	  val fullcashflow:Double = livePayoffLegs.map{case (d, p) => p.spotCoupon(mkt) * d.dayCount case _=> 0.0}.sum
      	  (cleanPrice, accruedAmount) match { 
      	    case (Some(p), Some(a)) => Some((fullcashflow - p - a) / (dc.yearFraction(mkt.valuedate, maturity) * p))
      	    case _ => None
      	  }
      }
      else dirtyPrice.flatMap{case p => getYield(p, dc, comp, freq, accuracy, maxIteration)}
	}
	
	/*	Returns each bond yield.
	 */
	def yieldContinuous:Option[Double] = bondYield(Compounding.Continuous)
	
	def yieldSemiannual:Option[Double] = bondYield(Compounding.Compounded, Frequency.Semiannual)
	
	def yieldAnnual:Option[Double] = bondYield(Compounding.Compounded, Frequency.Annual)
	
	def yieldSimple:Option[Double] = bondYield(Compounding.None, Frequency.Annual)
	
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
	
    def fxFrontier(target:Double, accuracy:Double, maxIteration:Int, vd:qlDate, paths:Int = 0):List[Option[Double]] = 
      if (market.isEmpty) List.fill(underlyings.size)(None)
      else {
        val mkt = market.get
        val bond = this.dateShifted(this.valueDate.get.sub(vd).toInt)
        
        underlyings.map(ul => {
          if (ul.size != 6 || ul.takeRight(3) == "JPY") None
            val ccy = ul take 3
            bond.defaultModel = this.defaultModel
            bond.market = mkt
            
			def priceFromFXmult(y:Double):Double = {
              bond.market = mkt.fxShifted(Map(ccy -> y))
              if (paths > 0) bond.model.collect{case m => m.mcPaths = paths}
              bond.dirtyPrice.getOrElse(Double.NaN)
            }
            
            val priceformula = (y:Double) => (priceFromFXmult(y) - target)
            val mult = NewtonRaphson.solve(priceformula, 1.01, accuracy, 0.01, maxIteration)
            mult.collect{case m => mkt.fx(ccy, "JPY").getOrElse(Double.NaN) / m}
          })
        }
    
    def fxFrontiers:List[List[Option[Double]]] = fxFrontiers(1.00, 0.001, 20)
      
    def fxFrontiers(target:Double, accuracy:Double, maxIteration:Int, paths:Int = 0):List[List[Option[Double]]] = {
      val vds = (schedule zip bermudan).zipWithIndex.filter{case ((_, t), _) => t == true}.map{case ((d, _), index) => (d.paymentDate, index)}.toList.sortBy(_._1).reverse
      val tempTrigger = scala.collection.mutable.ArrayBuffer(trigger:_*)
      println("FX frontiers : " + id)
      
      vds.foreach{case (vd, index) => 
        val tempBond = triggerShifted(tempTrigger.toList)
        tempBond.market = this.market.orNull
        tempBond.defaultModel = this.defaultModel
        
        if (trigger(index).isEmpty) {
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
	  case (Some(vd), Some(curve)) => Some(livePayoffs._1.map{
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
	def macaulayDuration:Option[Double] = (valueDate, discountCurve) match {
	  case (Some(vd), Some(curve)) => {
	  	val (yearfrac, price):(List[Double], List[Double]) = spotFixedRates.map{case (d, r) => 
	  	  ((new Actual365Fixed).yearFraction(vd, d.paymentDate), r * d.coefficient(curve))}.unzip
	  	Some((yearfrac, price).zipped.map(_ * _).sum / price.sum)
	  }
	  case _ => None
	}
	
	/*	
	 * Returns modified duration defined as Macauley duration / (1 + yield / freq)
	 */
	def modifiedDuration:Option[Double] = modifiedDuration(Compounding.Compounded, Frequency.Annual)
	def modifiedDuration(comp:Compounding, freq:Frequency):Option[Double] = macaulayDuration.flatMap { case dur =>
	    comp match {
	      case Compounding.Continuous => Some(dur)
	      case Compounding.Compounded | Compounding.SimpleThenCompounded => bondYield(comp, freq).collect{case y => dur / (1.0 + y / freq.toInteger.toDouble)}
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
	  livePayoffs._2.variables.foreach {
	    case c if c.size == 6 => if (Currencies contains (c take 3)) result += (c take 3)
			  				  if (Currencies contains (c takeRight 3)) result += (c takeRight 3)
	    case c if c.size >= 3 => if (Currencies contains (c take 3)) result += (c take 3)
	    case _ => {}
	  }
	  (result + currency.code).toSet
	}
	
	/*	
	 * Returns rate delta
	 */
	def rateDelta(shift:Double):Option[Double] = rateDelta(currency.code, shift)
	
	def rateDelta(ccy:String, shift:Double):Option[Double] = rateDelta((b:Bond) => b.dirtyPrice, Map(ccy -> shift))
	  
	def rateDelta(target:Bond => Option[Double], shift:Map[String, Double]):Option[Double] = market.flatMap { case mkt =>
	  val initprice = target(this)
	  val newBond = this.clone
	  newBond.calibrationCache ++= this.calibrationCache
	  newBond.defaultModel = this.defaultModel
	  newBond.market = mkt.rateShifted(shift)
	  val newprice = target(newBond)
	  (initprice, newprice) match { case (Some(i), Some(n)) => Some(n - i) case _ => None }
	}
	
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
	  
	def fxDelta(target:Bond => Option[Double], mult:Map[String, Double]):Option[Double] = market.flatMap { case mkt =>
	  val initprice = target(this)
	  val newBond = this.clone
	  newBond.calibrationCache ++= this.calibrationCache
	  newBond.defaultModel = this.defaultModel
	  newBond.market = mkt.fxShifted(mult)
	  val newprice = target(newBond)
	  (initprice, newprice) match { case (Some(i), Some(n)) => Some(n - i) case _ => None }
	}
	
	/*	
	 * List of FX underlyings
	 */
	def fxList:Set[String] = livePayoffs._2.variables.filter(
	  c => ((c.size == 6) && (Currencies contains (c take 3)) && (Currencies contains (c takeRight 3))))

	/*	
	 * Returns rate vega
	 */
	def fxVegas(addvol:Double):Map[String, Double] = fxList.map(fx => (fx, fxVega(fx, addvol))).collect{case (a, Some(b)) => (a, b)}.toMap
	
	def fxVega(ccypair:String, addvol:Double):Option[Double] = fxVega((b:Bond) => b.dirtyPrice, Map(ccypair -> addvol))
	  
	def fxVega(target:Bond => Option[Double], addvol:Map[String, Double]):Option[Double] = market.flatMap { case mkt =>
	  val initprice = target(this)
	  val newBond = this.clone
	  newBond.calibrationCache ++= this.calibrationCache
	  newBond.defaultModel = this.defaultModel
	  newBond.market = mkt.fxVolShifted(addvol)
	  val newprice = target(newBond)
	  (initprice, newprice) match { case (Some(i), Some(n)) => Some(n - i) case _ => None }
	}
	
	/*
     * Cash-flow convexity
     * The convexity of a string of cash flows is defined as {@latex[ C = \frac{1}{P} \frac{\partial^2 P}{\partial y^2} } where
     * {@latex$ P } is the present value of the cash flows according to the given IRR {@latex$ y }.
     */
    def formulaConvexity(comp:Compounding, freq:Frequency = Frequency.Annual):Option[Double] = (valueDate, discountCurve) match {
	  case (Some(vd), Some(discount)) => 
	    val currentRate:Double = if (comp == Compounding.Compounded) bondYield(comp, freq).getOrElse(Double.NaN) else 0.0
	    if (currentRate.isNaN) return None
	    val f:Double = freq.toInteger.toDouble
	    val dc = new Actual365Fixed
	    val (p, d2Pdy2) = spotFixedRates.map{case (d, r) => 
	      val t = dc.yearFraction(vd, d.paymentDate)
	      val c = r * d.dayCount
	      val B = discount(d.paymentDate)
	      val d2:Double = comp match {
	        case Compounding.Simple => c * 2.0 * B * B * B * t * t
	        case Compounding.Compounded | Compounding.SimpleThenCompounded => c * B * t * (f * t + 1) / (f * (1 + currentRate / f) * (1 + currentRate / f))
	        case Compounding.Continuous => c * B * t * t
	      }
	      (c * B, d2)
	      }.unzip
	    if (p.sum == 0) None else Some(d2Pdy2.sum / p.sum)
	    
	  case _ => None
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
	
	val defaultMathContext = new JMC(34, RoundingMode.HALF_UP)
	
	def toCoupons:Set[dbCoupon] = {
	  val pos = allPayoffLegs
	  val spot:List[Double] = spotFixedRatesAll.unzip._2
	  
	  (0 to pos.size - 1).map(i => {
	      val (s, p) = pos(i)
	      val fixedrate = p match {case po:FixedPayoff => Some(po.payoff) case _ => None }
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
	
	def show:Unit = {
	    println("Id:" + id)
	    println("Currency: " + currency.code)
	    println("Model: " + (model match { case None => "Not defined" case Some(m) => m.getClass.getName}))
	    println("Market: " + (market match { case None => "Not defined" case Some(m) => m.paramset}))
	    
	    if (market isDefined) {
	      println("Live payoffs") 
	      livePayoffLegs.foreach{case (s, po) => println(s + " " + po)}
	      println("Triggers: " + liveTriggers.mkString(","))
	      println("Bermudans: " + liveBermudans.mkString(","))
	    }
	    else {
	      println("Full schedule:")
		  payoffLegs.foreach{case (s, po) => println(s + " " + po)}
	      println("Triggers: " + trigger.mkString(","))
	      println("Bermudans: " + bermudan.mkString(","))
	    }
	  }
	
	def showAll:Unit = {
	  def disp(name:String, f: => Any) = println(name + "\t" + (f match {
	    case s:Option[Any] => s.getOrElse("None")
	    case s => s.toString}))
	  show
	  disp("currency", currency.code)
	  disp("paramset", market.collect{case m => m.paramset})
	  disp("paramdate", market.collect{case m => m.valuedate.longDate})
	  disp("issuedate", issueDate)
	  disp("maturity", maturity)
	  disp("fx jpy spot", fxjpy)
	  disp("dirty price", dirtyPrice)
	  disp("clean price", cleanPrice)
	  disp("accrued cpn", accruedAmount)
	  disp("dirty JPY", dirtyPriceJpy)
	  disp("clean JPY", cleanPriceJpy)
	  disp("accruedJpy", accruedAmountJpy)
	  disp("yieldCont", yieldContinuous)
	  disp("yieldAnnual", yieldAnnual)
	  disp("yieldSemi", yieldSemiannual)
	  disp("yieldSimple", yieldSimple)
	  disp("bpValue  ", bpvalue)
	  disp("irr\t", irr)
	  disp("currentRate", currentRate)
	  disp("next amount", nextPayment)
	  disp("effectiveDuration", effectiveDuration)
	  disp("modified duration", modifiedDuration)
	  disp("macaulay duration", macaulayDuration)
	  disp("yieldValuebp", yieldValueBasisPoint)
	  disp("convexity", convexity)
	  disp("remainLife", remainingLife)
	  disp("parMtMYield", parMtMYield)
	  disp("parMtMfx", parMtMfx)
	  disp("rate delta 1%", mapToJsonString(rateDeltas(0.001)))
	  disp("fx delta 1%", mapToJsonString(fxDeltas(1.01)))
	  disp("fx delta 1yen", mapToJsonString(fxDeltaOneJpy))
	  disp("fx vega 1%", mapToJsonString(fxVegas(0.01)))
	  disp("initialfx", initialFX)
	  
	}
	
	/*	Returns message returned by pricing model.
	 */
	def modelmsg:Unit = model match { case None => {} case Some(m) => m.message.foreach(println) }
	
} 


object Bond {
  
	def apply(db:dbBond):Option[Bond] = {
	  
		val defaultDayCounter:DayCounter = new Actual365Fixed
		val defaultAdjustment:BusinessDayConvention = BusinessDayConvention.ModifiedFollowing
		
		val fixingMap:Map[String, Double] = 
		  if (db.fixings == null || db.fixings.isEmpty) Map.empty
		  else {
		    val vmap:JavaMap[String, Double] = (new ObjectMapper).readValue(db.fixings, new TypeReference[JavaMap[String, Double]]{})
		    vmap.toMap
		  }
		  
		def applyFixing(v:String):String = {
		    var result = v
		    fixingMap.foreach{case (k, d) => result = result.replace("@" + k, d.toString)}
		    result
		  }
		
		val id = db.id
		
		val currency:Currency = Currencies(db.currencyid).orNull
		if (currency == null) { return None}
		
		val period:qlPeriod = (db.coupon_freq collect { case f => new qlPeriod(f, TimeUnit.Months)}).orNull
		if (period == null) { return None}
		
		val issueDate:qlDate = new qlDate(db.issuedate)
		
		val maturity:qlDate = new qlDate(db.maturity)
		
		val daycount:DayCounter = Daycounters(db.daycount).getOrElse(defaultDayCounter)
		
		val calendarAdjust:BusinessDayConvention = DayAdjustments.getOrElse(db.daycount_adj, defaultAdjustment)
		
		val paymentAdjust:BusinessDayConvention = DayAdjustments.getOrElse(db.payment_adj, defaultAdjustment)
		
		val maturityAdjust:BusinessDayConvention = DayAdjustments.getOrElse(db.daycount_adj, defaultAdjustment)
	
		val calendar:Calendar = db.calendar
		
		val fixingInArrears:Boolean = db.inarrears match {
		  case Some(0) => false
		  case _ => true
		}
		
		val couponNotice:Int = db.cpnnotice.getOrElse(5)
		
		val rule:DateGeneration.Rule = DateGeneration.Rule.Backward
		
		val firstDate:Option[qlDate] = None
		 
		val nextToLastdate:Option[qlDate] = None
		
		val issuer:String = db.issuerid
		
		val redemnotice = db.redemnotice.getOrElse(couponNotice)
		
		val schedule:Schedule = try {
			Schedule(issueDate, maturity, period, calendar, calendarAdjust, paymentAdjust, 
			    maturityAdjust, rule, fixingInArrears, couponNotice, daycount, firstDate, 
			    nextToLastdate, true, redemnotice)}
			  catch { case _ => null}
		      
		if (schedule == null) { return None}

		if (db.redemprice == null || db.redemprice.isEmpty) {return None}
		val redemption:Payoff = Payoff(applyFixing(db.redemprice))
		
		val calllist = applyFixing(db.call).jsonNode
		
		val bermudan:List[Boolean] = calllist match {
		  case Some(b) if b.isArray && b.size == 1 => List.fill(schedule.size - 2)(b.head.parseJsonString == "berm") ++ List(false, false)
		  case Some(b) if b isArray => List.fill(schedule.size - b.size - 2)(false) ++ b.map(_.parseJsonString == "berm").toList ++ List(false, false)
		  case _ => List.fill(schedule.size)(false)
		}
		
		val underlyings:List[String] = db.underlying.jsonNode match {
		  case Some(n) if n isArray => n.map(_.parseJsonString).toList
		  case _ => List.empty
		}
		
		val trigger:List[List[Option[Double]]] = calllist match {
		  case Some(b) if b.isArray && b.size == 1 => 
		    List.fill(schedule.size - 2)(if (b.head isArray) b.head.map(_.parseJsonDouble).toList else List.empty) ++ List.fill(2)(List.empty)
		  case Some(b) if b isArray => 
		    List.fill(schedule.size - b.size - 2)(List.empty) ++ 
		    b.map(n => if (n isArray) n.map(_.parseJsonDouble).toList else List.empty) ++ List.fill(2)(List.empty)
		  case _ => List.fill(schedule.size)(List.empty)
		}
		
		val coupon:Payoffs = if (db.coupon == null || db.coupon.isEmpty) null
			else Payoffs(applyFixing(db.coupon), schedule.size - 1)
			
		if (coupon == null) {return None}
		if (coupon.size + 1 != schedule.size) {return None}
		
		Some(Bond(db, schedule, coupon, redemption, underlyings, bermudan, trigger))
	  
	}
  
}
	
