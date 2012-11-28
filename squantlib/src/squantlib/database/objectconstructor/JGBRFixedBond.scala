package squantlib.database.objectconstructor

import squantlib.database.schemadefinitions.{Bond => dbBond}
import squantlib.setting.initializer.{Currencies, DayAdjustments, Daycounters}
import squantlib.jquantlib.instruments.bonds.JGBFixedBond
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod, TimeUnit, Schedule, DateGeneration, BusinessDayConvention}
import org.jquantlib.daycounters.Actual365Fixed
import squantlib.jquantlib.pricingengines.bond.JGBRBondEngine

object JGBRFixedBond {
  
	def apply(bond:dbBond) = getbond(bond)
	def apply(bond:dbBond, valuedate:qlDate) = getbond(bond, valuedate)
  
	val productlist = Set("JGBR3", "JGBR5")
	def isCompatible(bond:dbBond) = productlist contains bond.productid.toUpperCase
	
	val defaultAdjustment = BusinessDayConvention.ModifiedFollowing
	val defaultDayCounter = new Actual365Fixed
	
	def getbonds(bonds:Set[dbBond]):Map[String, JGBFixedBond] = {
	  bonds.map(b => (b.id, getbond(b))).collect{case (key, Some(b)) => (key, b)}.toMap
	}
	
	def getbonds(bonds:Set[dbBond], valuedate:qlDate):Map[String, JGBFixedBond] = {
	  bonds.map(b => (b.id, getbond(b, valuedate))).collect{case (key, Some(b)) => (key, b)}.toMap
	}
	
	def getbond(bond:dbBond, valuedate:qlDate):Option[JGBFixedBond] = {
	  val newbond = getbond(bond)
	  if (newbond.isEmpty) None
	  else {
	    setDefaultPricingEngine(newbond.get, valuedate)
	    newbond
	  }
	}
	
	def getbond(bond:dbBond):Option[JGBFixedBond] = {
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
			val redemption = try{bond.redemprice.trim.toDouble} catch { case _ => Double.NaN}
			val initialfx = bond.initialfx
			
			val newbond = new JGBFixedBond(settlementdays, 
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

	def setDefaultPricingEngine(bond:JGBFixedBond, valuedate:qlDate) ={
	  if (bond != null) 
	    bond.setPricingEngine(new JGBRBondEngine(valuedate), valuedate)
	}
	
	def getAdjustedPricingEngine(bond:JGBFixedBond, newvaluedate:qlDate):Option[JGBRBondEngine] = 
		try { Some(new JGBRBondEngine(newvaluedate)) } 
		catch { case e:Exception => None}
	
	def setAdjustedPricingEngine(bond:JGBFixedBond, newvaluedate:qlDate):Unit = {
	  getAdjustedPricingEngine(bond, newvaluedate) match {
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