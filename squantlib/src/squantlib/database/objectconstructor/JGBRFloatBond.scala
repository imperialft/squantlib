package squantlib.database.objectconstructor

import squantlib.database.IndexValue
import squantlib.database.schemadefinitions.{Bond => dbBond}
import squantlib.initializer.{Currencies, DayAdjustments, Daycounters}
import squantlib.instruments.bonds.JGBFloatBond
import squantlib.pricingengines.bond.JGBRBondEngine
import squantlib.cashflow.JGBRFloatingCouponPricer
import squantlib.math.Payoff
import scala.collection.JavaConversions._
import squantlib.initializer.BondYieldIndices
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod, TimeUnit, Schedule, DateGeneration, BusinessDayConvention}
import org.jquantlib.daycounters.Actual365Fixed
import org.jquantlib.indexes.JpyJGBYieldIndex
import org.jquantlib.math.matrixutilities.{Array => qlArray}
import org.jquantlib.cashflow.CappedFlooredCmtCoupon

object JGBRFloatBond {
  
	def apply(bond:dbBond) = getbond(bond)
	def apply(bond:dbBond, valuedate:qlDate) = getbond(bond, valuedate)
  
	val productlist = Set("JGBR10F", "JGBR10N")
	def isCompatible(bond:dbBond) = productlist contains bond.productid.toUpperCase
	
	val defaultAdjustment = BusinessDayConvention.ModifiedFollowing
	val defaultDayCounter = new Actual365Fixed
	
	def getbonds(bonds:Set[dbBond]):Map[String, JGBFloatBond] = {
	  bonds.map(b => (b.id, getbond(b))).collect{case (key, Some(b)) => (key, b)}.toMap
	}
	
	def getbonds(bonds:Set[dbBond], valuedate:qlDate):Map[String, JGBFloatBond] = {
	  bonds.map(b => (b.id, getbond(b, valuedate))).collect{case (key, Some(b)) => (key, b)}.toMap
	}
	
	def getbond(bond:dbBond, valuedate:qlDate):Option[JGBFloatBond] = {
	  val newbond = getbond(bond)
	  if (newbond.isEmpty) None
	  else {
	    setDefaultPricingEngine(newbond.get, valuedate)
	    newbond
	  }
	}
	
	def getbond(bond:dbBond):Option[JGBFloatBond] = {
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
			val accrualdaycounter = Daycounters.getOrElse(bond.daycount, defaultDayCounter)
			val paymentconvention = DayAdjustments.getOrElse(bond.payment_adj, defaultAdjustment)
			val redemption = try{bond.redemprice.trim.toDouble} catch { case _ => Double.NaN}
			val initialfx = bond.initialfx
			
			val coupons:Array[Payoff] = ratetoarray(bond.coupon, schedule.size - 1)
			
			val cmtname = {
				val v = coupons.map(_.variables).flatten.toSet
				if (v.size != 1) null else v.head
			}
			if (cmtname == null) return None
			
			val index = BondYieldIndices(cmtname).orNull
			if (index == null) return None
			
			val nullrate:Double = org.jquantlib.math.Constants.NULL_RATE
			val gearings:Array[Double] = coupons.map(c => c.leverage(cmtname))
			val spreads:Array[Double] = coupons.map(c => c.constant)
			
			val caps:Array[Double] = {
			  val extract = coupons.map(c => c.cap)
			  if (extract.forall(_.isEmpty)) Array.empty else extract.map(_.getOrElse(nullrate))
			}
			
			val floors:Array[Double] = {
			  val extract = coupons.map(c => c.floor)
			  if (extract.forall(_.isEmpty)) Array.empty else extract.map(_.getOrElse(nullrate))
			}
			
			implicit def arrayConverter(a:Array[Double]):qlArray = new qlArray(a)
			
			val newbond = new JGBFloatBond(
					settlementDays = settlementdays,
			        faceAmount = faceamount,
			        schedule = schedule,
			        index = index,
			        paymentDayCounter = accrualdaycounter,
			        paymentConvention = paymentconvention,
			        fixingDays = settlementdays,
			        gearings = gearings,
			        spreads = spreads,
			        caps = caps,
			        floors = floors,
			        inArrears = false,
			        redemption = redemption,
			        issueDate = issuedate,
			        id = bondid, 
			        currency = currency,
			        creditSpreadID = issuerid,
			        initialFX = initialfx)
			
			Some(newbond)
	  	}
	}

	def setDefaultPricingEngine(bond:JGBFloatBond, valuedate:qlDate):Unit = {
		if (bond == null) return
	  
	    bond.setPricingEngine(new JGBRBondEngine(valuedate), valuedate)
	    
		val lastfixing = IndexValue(bond.refindex, valuedate.longDate) match {
		  case Some((d, r)) => r
		  case _ => Double.NaN
		}
		
		bond.cashflows.foreach{
		  case c:CappedFlooredCmtCoupon => c.setPricer(new JGBRFloatingCouponPricer(c, _ => lastfixing))
		  case _ => {}
		}
	}

	private def ratetoarray(formula:String, size:Int):Array[Payoff] = {
		val formulaarray = formula.split(";")
		    
		(0 to (size-1)).map(i => {
		  val m = size - formulaarray.size
		  new Payoff(if(i < m) formulaarray(0) else formulaarray(i - m))
		  }).toArray
	}
	
}