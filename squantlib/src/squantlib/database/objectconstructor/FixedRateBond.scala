package squantlib.database.objectconstructor

import squantlib.model.CurveFactory
import squantlib.database.schemadefinitions.{Bond => dbBond}
import squantlib.setting.initializer.{Currencies, DayAdjustments, Daycounters}
import squantlib.payoff.FixedPayoff
import org.jquantlib.instruments.bonds.{FixedRateBond => qlFixedRateBond }
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod, TimeUnit, Schedule, DateGeneration, BusinessDayConvention, Calendar}
import org.jquantlib.daycounters.Actual365Fixed
import org.jquantlib.pricingengines.bond.DiscountingBondEngine

object FixedRateBond {
  
	def apply(bond:dbBond) = getbond(bond)
	def apply(bond:dbBond, factory:CurveFactory) = getbond(bond, factory)
  
	val productlist = Set("SB", "STEPUP", "DISC")
	def isCompatible(bond:dbBond) = productlist contains bond.productid.toUpperCase
	
	val defaultAdjustment = BusinessDayConvention.ModifiedFollowing
	val defaultDayCounter = new Actual365Fixed
	
	def getbonds(bonds:Set[dbBond]):Map[String, qlFixedRateBond] = {
	  bonds.map(b => (b.id, getbond(b))).collect{case (key, Some(b)) => (key, b)}.toMap
	}
	
	def getbonds(bonds:Set[dbBond], factory:CurveFactory):Map[String, qlFixedRateBond] = {
	  bonds.map(b => (b.id, getbond(b, factory))).collect{case (key, Some(b)) => (key, b)}.toMap
	}
	
	def getbond(bond:dbBond, factory:CurveFactory):Option[qlFixedRateBond] = getbond(bond) match {
	  case Some(b) => {setDefaultPricingEngine(b, factory); Some(b)}
	  case None => None
	}
	
	def getbond(bond:dbBond):Option[qlFixedRateBond] = {
	  val isvalidbond = productlist.contains(bond.productid) && 
			  		!bond.coupon.isEmpty && 
			  		!bond.coupon_freq.isEmpty && 
			  		!bond.redemprice.isEmpty && 
			  		!bond.daycount_adj.isEmpty
	  
	  if (!isvalidbond) None
	  else {
	  		val bondid = bond.id
	  		val issuerid = bond.issuerid
	  		val issuedate = new qlDate(bond.issuedate)
			val maturity = new qlDate(bond.maturity)
			val schedule = {
			  val tenor = new qlPeriod(bond.coupon_freq.get, TimeUnit.Months)
			  val calendar = bond.calendar
			  val convention = DayAdjustments.getOrElse(bond.daycount_adj, defaultAdjustment)
			  val maturityconvention = DayAdjustments.getOrElse(bond.daycount_adj, defaultAdjustment)
			  val rule = DateGeneration.Rule.Backward
			  val endofmonth = false
			  new Schedule(issuedate, maturity, tenor, calendar, convention, maturityconvention, rule, endofmonth)
			}
			
			val currency = Currencies.getOrElse(bond.currencyid, null)
			val settlementdays = 0
			val faceamount = 100.0
			val coupons:Array[Double] = ratetoarray(bond.coupon, schedule.size - 1)
			val accrualdaycounter = Daycounters.getOrElse(bond.daycount, defaultDayCounter)
			val paymentconvention = DayAdjustments.getOrElse(bond.payment_adj, defaultAdjustment)
			
			val redemption = FixedPayoff(bond.redemprice).price * 100.0
			
			val initialfx = bond.initialfx
			
			val newbond = new qlFixedRateBond(settlementdays, 
			    faceamount, 
			    schedule, 
			    coupons, 
			    accrualdaycounter, 
			    paymentconvention, 
			    redemption, 
			    issuedate, 
			    bondid, 
			    currency, 
			    issuerid, 
			    initialfx)
			
			Some(newbond)
	  	}
	}

	def setDefaultPricingEngine(bond:qlFixedRateBond, factory:CurveFactory) ={
	  if (bond != null && factory != null) 
	    bond.setPricingEngine(factory.getDiscountBondEngine(bond).orNull, factory.valuedate)
	}
	
	def getAdjustedPricingEngine(bond:qlFixedRateBond, factory:CurveFactory, newvaluedate:qlDate):Option[DiscountingBondEngine] = 
		try { 
		  val newcurve = factory.getDiscountCurve(bond.currency.code, bond.creditSpreadID).get
		  newcurve.valuedate = newvaluedate
		  Some(newcurve.toDiscountBondEngine(bond.calendar)) } 
		catch { case _ => None}
	
	def setAdjustedPricingEngine(bond:qlFixedRateBond, factory:CurveFactory, newvaluedate:qlDate):Unit = {
	  getAdjustedPricingEngine(bond, factory, newvaluedate) match {
	    case Some(engine) => bond.setPricingEngine(engine, newvaluedate)
	    case None => {}
	  }
	}

	private def ratetoarray(formula:String, size:Int):Array[Double] = {
		val numarray = formula.split(";").map(x => (
		    try{x.replace("%", "").trim.toDouble / 100.0} 
		    catch { case _ => Double.NaN }))
		    
		(0 to (size-1)).map(i => {
		  val m = size - numarray.size
		  if(i < m) numarray(0) else numarray(i - m)
		  }).toArray
	}
	
}