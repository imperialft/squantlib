package squantlib.model

import org.jquantlib.currencies.Currency
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod, TimeUnit, _}
import org.jquantlib.termstructures.Compounding
import org.jquantlib.daycounters.{Absolute, Actual365Fixed, Thirty360, DayCounter}
import squantlib.database.schemadefinitions.{Bond => dbBond}
import squantlib.payoff.{Payoffs, Schedule, Payoff, CalcPeriod}
import squantlib.model.rates.DiscountCurve
import squantlib.setting.initializer.{DayAdjustments, Currencies, Daycounters}
import squantlib.util.JsonUtils._
import squantlib.database.fixings.Fixings
import squantlib.pricing.model.{PricingModel, NoModel}
import squantlib.math.solver.NewtonRaphson
import org.codehaus.jackson.JsonNode

/**
 * Bond class with enclosed risk analysis functions.
 */
class Bond(
		val db:dbBond, 
	    val id:String,	
		val issueDate:qlDate,
		val maturity:qlDate,	
		val currency:Currency,
		val nominal:Double,	
		val denomination:Option[Double],	
		val period:qlPeriod,	
		val daycount:DayCounter,	
		val calendarAdjust:BusinessDayConvention,	
		val paymentAdjust:BusinessDayConvention,	
		val maturityAdjust:BusinessDayConvention,
		val calendar:Calendar,	
		val fixingInArrears:Boolean,	
		val couponNotice:Int,	
		val issuePrice:Option[Double],	
		val call:String,	
		val bondType:String,	
		val initialFX:Double,	
		val issuer:String,	
		val inputSchedule:Schedule,	      
		val coupon:Payoffs,	
		val redemption:Payoff,	
		val settings:Option[JsonNode]) {
  
  
	/* 
	 * Specify default pricing model
	 */
	private var _model:Option[PricingModel] = None
	def model:Option[PricingModel] = _model
		
	def initializeModel = 
	  _model = if (modelSetter == null || market.isEmpty) None else modelSetter(market.get, this)
	
	private var _modelSetter:(Market, Bond) => Option[PricingModel] = null
	
	def modelSetter = _modelSetter
	def modelSetter_= (newModel:(Market, Bond) => Option[PricingModel]) = {
		_modelSetter = newModel
		initializeModel
	}
	
	private var _market:Option[Market] = None
	
	/* 
	 * Specify default market parameters
	 */
	def market:Option[Market] = _market
	def market_= (newMarket:Market) = {
	  _market = Some(newMarket)
		initializeModel
	}
	
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
	def livePayoffs:(Schedule, Payoffs) = valueDate.collect {case d => livePayoffs(d)}.orNull
	
	def livePayoffs(vd:qlDate):(Schedule, Payoffs) = {
		val payoffSchedule = payoffLegs.filter{case (cp, p) => (cp.paymentDate gt vd)}
	  
    	val po = payoffSchedule.map{
    	  case (_, payoff) if payoff.variables.size == 0 => payoff
    	  case (period, payoff) if period.eventDate gt vd => payoff
    	  case (period, payoff) => {
    	    val fixings = payoff.variables.map(v => Fixings(v, period.eventDate).collect{
    	      case (d, f) => (v, f)}).flatMap(x => x).toMap
    	    payoff.applyFixing(fixings)
    	    }
    	}
    	(Schedule(payoffSchedule.unzip._1), Payoffs(po))
	}
	
	/*	
	 * Returns "live" payment schedules broken down into pairs of a Calculation Period and a Payoff
	 *  @param value date
	 * 	@returns element 1: Schedule containing legs with payment date after market or specified value date
	 * 			element 2: Payoffs containing legs with payment dates after market or specified value date
	 */
	def livePayoffLegs:List[(CalcPeriod, Payoff)] = market.collect { case m => livePayoffs match { case (s, p) => (s.toList zip p.toList)} }.orNull
	
	def livePayoffLegs(vd:qlDate):List[(CalcPeriod, Payoff)] = livePayoffs(vd) match { case (s, p) => (s.toList zip p.toList)}
	
	/*	
	 * Returns discount curve.
	 * 	@returns discount curve created from either pre-set or specified market
	 */
	def discountCurve:Option[DiscountCurve] = market.flatMap(m => discountCurve(m))
	
	def discountCurve(mkt:Market):Option[DiscountCurve] = mkt.getDiscountCurve(currency, issuer)

	/*	
	 * Returns discount curve.
	 * 	@returns discount curve created from either pre-set or specified market
	 */
	def discountFactors:Option[List[(qlDate, Double)]] = market.flatMap(m => discountFactors(m))
	
	def discountFactors(mkt:Market):Option[List[(qlDate, Double)]] = discountCurve(mkt)
		.collect{case curve => schedule.paymentDates.withFilter(_ gt mkt.valuedate).map(d => (d, curve(d)))}
	
	/*	
	 * Returns coupons fixed with current spot market (not forward!). 
	 */
	def spotFixedRates(mkt:Market):List[(CalcPeriod, Double)] = livePayoffLegs.map{case (d, p) => (d, p.spotCoupon(mkt))}
	
	def spotFixedRates:List[(CalcPeriod, Double)] = if (market isDefined) spotFixedRates(market.get) else livePayoffLegs.map{case (d, p) => (d, Double.NaN)}
	
	/*	
	 * Returns forward value of each coupon (not discounted)
	 */
	def forwardLegs:Option[List[(CalcPeriod, Double)]] = valueDate.flatMap { case d => 
	    val (dates, payoff) = livePayoffs(d)
	    val pricemodel = if (payoff.variables.size == 0) Some(NoModel(payoff, dates)) else model
	    pricemodel match {
	      case None => println(id + " : model calibration error"); None
	      case Some(mdl) => Some((dates zip mdl.priceLegs).toList)
	    } 
	}
	
	def forwardLegs(mkt:Market):Option[List[(CalcPeriod, Double)]] = {
	    val (dates, payoff) = livePayoffs(mkt.valuedate)
	    val pricemodel = if (payoff.variables.size == 0) Some(NoModel(payoff, dates)) else modelSetter(mkt, this)
	    pricemodel match {
	      case None => println(id + " : model calibration error"); None
	      case Some(m) => Some((dates zip m.priceLegs).toList)
	    } 
	}
	
	/*	
	 * Returns price legs of the bond. (including accrued interest)
	 */
	def priceLegs:Option[List[Double]] = (discountCurve, forwardLegs) match {
	  case (Some(curve), Some(fwd)) => Some(fwd.map{ case (d, p) => d.coefficient(curve) * p})
	  case _ => None
	}
	
	def priceLegs(mkt:Market):Option[List[Double]] = (discountCurve(mkt), forwardLegs(mkt)) match {
	  case (Some(curve), Some(fwd)) => Some(fwd.map{ case (d, p) => d.coefficient(curve) * p})
	  case _ => None
	}
	
	/*	
	 * Returns dirty price of the bond. (ie. including accrued interest)
	 */
	def dirtyPrice:Option[Double] = priceLegs.collect{case legs => legs.reduceLeft{_ + _}}
	
	def dirtyPrice(mkt:Market):Option[Double] = priceLegs(mkt).collect{case legs => legs.reduceLeft{_ + _}}
	
	/*	
	 * Returns clean price of the bond (ie. Dirty price - accrued coupon)
	 */
	def cleanPrice:Option[Double] = (dirtyPrice, accruedAmount) match { case (Some(d), Some(a)) => Some(d - a) case _ => None}
	
	def cleanPrice(mkt:Market):Option[Double] = (dirtyPrice(mkt), accruedAmount(mkt)) match { case (Some(d), Some(a)) => Some(d - a) case _ => None}
	
	/*	
	 * Returns accrued coupon.
	 */
	def accruedAmount(mkt:Market):Option[Double] = Some(payoffLegs.withFilter{case (d, p) => (d.isCurrentPeriod(mkt.valuedate) && d.daycounter != new Absolute)}
	  								.map{case (d, p) => (d.accrued(mkt.valuedate)) * p.spotCoupon(mkt) }.reduceLeft{_ + _})

	def accruedAmount:Option[Double] = market.flatMap(m => accruedAmount(m))
	
	/*	
	 * Returns current coupon rate.
	 */
	def currentRate(mkt:Market):Option[Double] = Some(payoffLegs.withFilter{case (d, p) => (d.isCurrentPeriod(mkt.valuedate) && d.daycounter != new Absolute)}
	  								.map{case (d, p) => p.spotCoupon(mkt) }.reduceLeft{_ + _})

	def currentRate:Option[Double] = market.flatMap(m => currentRate(m))
	
	/*	
	 * Returns accrued coupon.
	 */
	def nextPayment(mkt:Market):Option[(qlDate, Double)] = Some(payoffLegs.filter{case (d, p) => (d.isCurrentPeriod(mkt.valuedate) && d.daycounter != new Absolute)}
									.minBy{case (d, p) => d.paymentDate}
									match {case (d, p) => (d.paymentDate, d.dayCount * p.spotCoupon(mkt))})

	def nextPayment:Option[(qlDate, Double)] = market.flatMap(m => nextPayment(m))
	
	/*	
	 * Returns spot FX rate against JPY
	 */
	def fxjpy:Option[Double] = market match {case Some(mkt) => mkt.fx(currency.code, "JPY") case _ => None}
	
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
    def getYield(target:Double, dc:DayCounter, comp:Compounding, freq:Frequency, accuracy:Double, maxIteration:Int):Option[Double] = valueDate.flatMap{ case vd =>
      if (comp == Compounding.None) return None
	  
	  val paylegs:List[(Double, Double)] = spotFixedRates.map{case (d, r) => (dc.yearFraction(vd, d.paymentDate), r * d.dayCount)}
	  if (paylegs.exists(_._2.isNaN)) return None
	    
	  def priceFromYield(y:Double):Double = {
	    def zc(d:Double) = comp match {
	      case Compounding.Simple => 1.0 / (1.0 + y * d)
	      case Compounding.Compounded | Compounding.SimpleThenCompounded => {val fr = freq.toInteger.toDouble; 1.0 / math.pow(1.0 + y / fr, fr * d)}
	      case Compounding.Continuous => math.exp(-y * d)
	      } 
	      paylegs.map{case (d, v) => v * zc(d)}.reduceLeft{_ + _}
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
      if (comp == Compounding.None) 
      	market.flatMap {case mkt => 
      	  val fullcashflow:Double = livePayoffLegs.map{case (d, p) => p.spotCoupon(mkt) * d.dayCount case _=> 0.0}.reduceLeft{_ + _}
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
	
	/*	Returns yield at which bond price becomes 100% (if any)
	 * @param comp Compounding rate, as one of the following
	 * 		"None" => Not applicable
	 * 		"Simple" => No compounding : ZC = 1 / rt
	 * 		"Compounded" => Standard compounding: ZC = 1 / (1+r/f)^tf 	
	 * 		"Continuous" => 
	 */
	def parMtMYield:Option[Double] = getYield(1.0, daycount, Compounding.Continuous, null, 0.00001, 20)
	
	/*	Returns present value of adding 1 basis point of coupon for the remainder of the bond.
	 */
	def bpvalue:Option[Double] = (valueDate, discountCurve) match {
	  case (Some(vd), Some(curve)) => Some(livePayoffs._1.map{
	    case d if d.daycounter == new Absolute => 0.0
	    case d => d.dayCountAfter(vd) * curve(d.paymentDate)
	  }.sum * 0.0001) 
	  case _ => None
	}
	
	/*	Internal Rate of Return, defined to be the same as annually compounded yield.
	 */
    def irr:Option[Double] = irr(daycount, 0.00001, 20)
	
    def irr(dc:DayCounter, accuracy:Double, maxIteration:Int):Option[Double] = yieldAnnual
    
    /*
     * Yield value of a basis point The yield value of a one basis point change
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
	  	  (daycount.yearFraction(vd, d.paymentDate), r * d.coefficient(curve))}.unzip
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
	 * Returns modified duration defined as rate delta
	 */
	def effectiveDuration:Option[Double] = None // TO BE COMPUTED AS RATE DELTA
	
    /*
     * Cash-flow convexity
     * The convexity of a string of cash flows is defined as {@latex[ C = \frac{1}{P} \frac{\partial^2 P}{\partial y^2} } where
     * {@latex$ P } is the present value of the cash flows according to the given IRR {@latex$ y }.
     */
    def convexity(comp:Compounding, freq:Frequency = Frequency.Annual):Option[Double] = (valueDate, discountCurve) match {
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
	
	def convexity:Option[Double] = convexity(Compounding.Continuous, Frequency.Annual)
	
    /*
     * Remaining life in number of years
     */
	
	def remainingLife:Option[Double] = valueDate.collect{ case d => (new Actual365Fixed).yearFraction(d, maturity)}
	
	
	
	override def toString:String = "ID: " + id + "\n"
	
	def show:Unit = {
	    println("Id:" + id)
	    println("Currency: " + currency.code)
	    println("Default Model: " + (model match { case None => "Not defined" case Some(m) => m.getClass.getName}))
	    println("Default Market: " + (market match { case None => "Not defined" case Some(m) => m.paramset}))
	    
	    if (market isDefined) {
	      println("Remaining payoffs") 
	      livePayoffLegs.foreach{case (s, po) => println(s + " " + po)}
	    }
	    else {
	      println("Full Schedule:")
		  payoffLegs.foreach{case (s, po) => println(s + " " + po)}
	    }
	  }
	
	/*	Returns message returned by pricing model.
	 */
	def modelmsg:Unit = model match { case None => {} case Some(m) => m.message.foreach(println) }
	
} 


object Bond {
  
	def apply(db:dbBond):Option[Bond] = {
	  
		val defaultDayCounter:DayCounter = new Thirty360
		val defaultAdjustment:BusinessDayConvention = BusinessDayConvention.ModifiedFollowing
		
		val id = db.id
		
		val issueDate:qlDate = new qlDate(db.issuedate)
		
		val maturity:qlDate = new qlDate(db.maturity)
		
		val nominal:Double = db.nominal
		
		val currency:Currency = Currencies(db.currencyid).orNull
		if (currency == null) { println(db.id  + " : currency not found"); return None}
		
		val denomination:Option[Double] = db.denomination
		
		val period:qlPeriod = (db.coupon_freq collect { case f => new qlPeriod(f, TimeUnit.Months)}).orNull
		if (period == null) { println(db.id  + " : period not defined"); return None}
		
		val daycount:DayCounter = Daycounters(db.daycount).getOrElse(defaultDayCounter)
		
		val calendarAdjust:BusinessDayConvention = DayAdjustments.getOrElse(db.daycount_adj, defaultAdjustment)
		
		val paymentAdjust:BusinessDayConvention = DayAdjustments.getOrElse(db.payment_adj, defaultAdjustment)
		
		val maturityAdjust:BusinessDayConvention = DayAdjustments.getOrElse(db.daycount_adj, defaultAdjustment)
	
		val calendar:Calendar = db.calendar
		
		val fixingInArrears:Boolean = db.inarrears.isDefined && db.inarrears == 0
		
		val couponNotice:Int = db.cpnnotice.getOrElse(5)
		
		val issuePrice:Option[Double] = db.issueprice
		
		val call:String = db.call
		
		val bondType:String = db.bondtype
		
		val initialFX:Double = db.initialfx
		
		val rule:DateGeneration.Rule = DateGeneration.Rule.Backward
		
		val firstDate:Option[qlDate] = None
		 
		val nextToLastdate:Option[qlDate] = None
		
		val issuer:String = db.issuerid
		
		val redemnotice = db.redemnotice.getOrElse(10)
		
		val schedule:Schedule = try {
			Schedule(issueDate, maturity, period, calendar, calendarAdjust, paymentAdjust, 
			    maturityAdjust, rule, fixingInArrears, couponNotice, daycount, firstDate, 
			    nextToLastdate, true, redemnotice)}
			  catch { case _ => null}
		      
		if (schedule == null) { println(db.id  + " : schedule cannot be initialized"); return None}

		val coupon:Payoffs = if (db.coupon == null || db.coupon.isEmpty) null
			else Payoffs(db.coupon, schedule.size - 1)
			
		val redemption:Payoff = Payoff(db.redemprice)
		
		if (coupon == null) {println(id + " : coupon not defined"); return None}
		if (coupon.size + 1 != schedule.size) {println(id + " : coupon (" + (coupon.size+1) + ") and schedule (" + schedule.size + ")  not compatible"); return None}
		
		val settings:Option[JsonNode] = db.settings.jsonNode
		
		Some(new Bond(db, id, issueDate, maturity, currency, nominal, denomination, period, daycount,	
			calendarAdjust,	paymentAdjust,	maturityAdjust, calendar, fixingInArrears, couponNotice,	
			issuePrice,	call, bondType,	initialFX,	issuer,	schedule, coupon, redemption, settings))
	  
	}
  
}
	
