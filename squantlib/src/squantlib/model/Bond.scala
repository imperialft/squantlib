package squantlib.model

import org.jquantlib.currencies.Currency
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod, _}
import squantlib.database.schemadefinitions.{Bond => dbBond}
import squantlib.payoff.{Payoffs, Schedule, Payoff, CalcPeriod}
import squantlib.model.rates.DiscountCurve
import squantlib.setting.initializer.{DayAdjustments, Currencies, Daycounters}
import org.jquantlib.daycounters.Thirty360
import org.codehaus.jackson.JsonNode
import squantlib.util.JsonUtils._
import squantlib.database.fixings.Fixings
import squantlib.pricing.model.{PricingModel, NoModel}

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
  
  
	/* Specify default pricing model
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
	
	/* Specify default market parameters
	 */
	def market:Option[Market] = _market
	def market_= (newMarket:Market) = {
	  _market = Some(newMarket)
		initializeModel
	}
	
	/* schedule 
	 * @returns full bond payment schedule (date only)
	 * 
	 * payoffs
	 * @returns full bond payoff (no dates)
	 */
	val (schedule, payoffs):(Schedule, Payoffs) = inputSchedule.sortWith(coupon.toList :+ redemption) match { case (s, p) => (s, Payoffs(p.toList))}

	
	/*	Returns schedule & payoffs broken down into pairs of a Calculation Period + a Payoff
	 */
	val payoffLegs:List[(CalcPeriod, Payoff)] = (schedule.toList zip payoffs.toList).toList
	
	
	/*	Returns "live" schedules
	 * 	@returns Schedule containing legs with payment date after market value date or specified value date.
	 */
	def liveSchedule:Schedule = market.collect{case m => liveSchedule(m.valuedate)}.orNull
	
	def liveSchedule(valuedate:qlDate):Schedule = Schedule(schedule.toList.filter(_.paymentDate gt valuedate))
	
	
	/*	Returns "live" payment schedules
	 * 	@returns element 1: Schedule containing legs with payment date after market value date or specified value date.
	 * 			element 2: Payoffs containing legs with payment dates after market value date or specified value date.
	 */
	def livePayoffs:(Schedule, Payoffs) = market.collect {case m => livePayoffs(m.valuedate)}.orNull
	
	def livePayoffs(valuedate:qlDate):(Schedule, Payoffs) = {
		val payoffSchedule = payoffLegs.filter{case (cp, p) => (cp.paymentDate gt valuedate)}
	  
    	val po = payoffSchedule.map{
    	  case (_, payoff) if payoff.variables.size == 0 => payoff
    	  case (period, payoff) if period.eventDate gt valuedate => payoff
    	  case (period, payoff) => {
    	    val fixings = payoff.variables.map(v => Fixings(v, period.eventDate).collect{
    	      case (d, f) => (v, f)}).flatMap(x => x).toMap
    	    payoff.applyFixing(fixings)
    	    }
    	}
    	(Schedule(payoffSchedule.unzip._1), Payoffs(po))
	}
	
	/*	Returns "live" payment schedules broken down into pairs of a Calculation Period and a Payoff
	 *  @param value date
	 * 	@returns element 1: Schedule containing legs with payment date after market or specified value date
	 * 			element 2: Payoffs containing legs with payment dates after market or specified value date
	 */
	def livePayoffLegs:List[(CalcPeriod, Payoff)] = market.collect { case m => livePayoffs match { case (s, p) => (s.toList zip p.toList)} }.orNull
	
	def livePayoffLegs(valuedate:qlDate):List[(CalcPeriod, Payoff)] = livePayoffs(valuedate) match { case (s, p) => (s.toList zip p.toList)}
	
	
	/*	Returns discount curve.
	 * 	@returns discount curve created from either pre-set or specified market
	 */
	def discountCurve:Option[DiscountCurve] = market.flatMap(m => discountCurve(m))
	
	def discountCurve(mkt:Market):Option[DiscountCurve] = mkt.getDiscountCurve(currency, issuer)

	
	/*	Price with default market and model setter.
	 */
	def dirtyPrice:Option[Double] = (market, discountCurve) match {
	  case (None, _) => println(id + " : market not defined"); None
	  case (_, None) => println(id + " : discount curve not defined"); None

	  case (Some(mkt), Some(curve)) => 
	    val (dates, payoff) = livePayoffs(mkt.valuedate)
	    
	    val pricemodel = if (payoff.variables.size == 0) Some(NoModel(payoff, dates)) else model
	    				
	    pricemodel match {
	      case None => println(id + " : model calibration error"); None
	      case Some(mdl) => Some(mdl.priceWithDiscount(curve))
	    } 
	}
	
	/*	Price with either one of both of model setter and market specified.
	 */
	def dirtyPrice(msetter:(Market, Bond) => Option[PricingModel]):Option[Double] = market.flatMap(m => dirtyPrice(m, msetter))
	
	def dirtyPrice(mkt:Market):Option[Double] = if (modelSetter == null) None else dirtyPrice(mkt, modelSetter)
	
	def dirtyPrice(mkt:Market, msetter:(Market, Bond) => Option[PricingModel]):Option[Double] = discountCurve(mkt) match {
	  case None => println(id + " : discount curve not defined"); None
	  case Some(curve) => 
	    val (dates, payoff) = livePayoffs(mkt.valuedate)
	    
	    val pricemodel = if (payoff.variables.size == 0) Some(NoModel(payoff, dates)) else msetter(mkt, this)
	    				
	    pricemodel match {
	      case None => println(id + " : model calibration error"); None
	      case Some(m) => Some(m.priceWithDiscount(curve))
	    } 
	}
	
	def modelmsg:Unit = model match { case None => {} case Some(m) => m.message.foreach(println) }
	
	override def toString:String = "ID: " + id + "\n" + 
	  "Schedule:\n" + livePayoffLegs.map{case (s, po) => s + " " + po}.mkString("\n")
	  
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
	  
	  
//	def accruedAmount(mkt:Market) = payoffLegs
//									.withFilter{case (d, p) => d.isCurrentPeriod(mkt.valuedate)}
//	  								.map{case (d, p) => (d.accrued(mkt.valuedate),  }
//
    
//			val yield_continuous = validvalue(bond.`yield`(stddaycount, Compounding.Continuous, Frequency.NoFrequency))
//			val yield_annual = validvalue(bond.`yield`(stddaycount, Compounding.Compounded, Frequency.Annual))
//			val yield_semiann = validvalue(bond.`yield`(stddaycount, Compounding.Compounded, Frequency.Semiannual))
//			val yield_simple = validvalue(bond.`yield`(stddaycount, Compounding.None, Frequency.Annual))
//			val price_accrued = validvalue(bond.accruedAmount)
//			val price_clean = validvalue(bond.cleanPrice)
//			
//			var bps:Option[Double] = None
//			var atmrate:Option[Double] = None
//			var simpleduration:Option[Double] = None
//			var modifiedduration:Option[Double] = None
//			var macauleyduration:Option[Double] = None
//			var yieldvaluebp:Option[Double] = None
//			var convexity:Option[Double] = None
//			
//			val cfmodel = CashFlows.getInstance
//			val cashflows = bond.cashflows
//			val irr = validvalue(cfmodel.irr(cashflows, price, new Thirty360, Compounding.Continuous, Frequency.NoFrequency, valuedate, 0.001, 1000, 0.01))
//			val nextrate = validvalue(cfmodel.nextCouponRate(cashflows, valuedate))
//			val nextamount = validvalue(cfmodel.nextCashFlows(cashflows, valuedate).map(c => c.amount).sum)
//			val nextdate = Some(cfmodel.nextCashFlow(cashflows, valuedate).date.longDate)
//			val remaininglife = validvalue(bond.remainingLife)
//		
//			if (termstructure != null)
//			{
//				bps = validvalue(cfmodel.bps(cashflows, termstructure, valuedate))
//				atmrate = validvalue(cfmodel.atmRate(cashflows, termstructure, valuedate, valuedate, 0, 0))
//				
//				val interestrate = termstructure.forwardRate(valuedate, bond.maturityDate, termstructure.dayCounter, Compounding.Compounded, Frequency.Semiannual)
//				simpleduration = validvalue(cfmodel.duration(cashflows, interestrate, CashFlows.Duration.Simple, valuedate))
//				modifiedduration = validvalue(cfmodel.duration(cashflows, interestrate, CashFlows.Duration.Modified, valuedate))
//				macauleyduration = validvalue(cfmodel.duration(cashflows, interestrate, CashFlows.Duration.Macaulay, valuedate))
//				yieldvaluebp = validvalue(cfmodel.yieldValueBasisPoint(cashflows, interestrate, valuedate))
//				convexity = validvalue(cfmodel.convexity(cashflows, interestrate, valuedate))
//			}
//			
//			val initialfx = bond.initialFX
//			
//			val pricedirty_jpy = if (bond.issueDate ge valuedate) None
//			  					 else if (initialfx > 0) Some(price * fx / initialfx) 
//								 else None
//								 
//			val priceclean_jpy = if (bond.issueDate ge valuedate) None
//			  					 else if (price_clean.isDefined && initialfx > 0) Some(price_clean.get * fx / initialfx) 
//			  					 else None
//			  					 
//			val accrued_jpy = if (price_accrued.isDefined && initialfx > 0) Some(price_accrued.get * fx / initialfx) else None

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
	
