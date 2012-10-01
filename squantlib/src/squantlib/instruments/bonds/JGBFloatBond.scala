package squantlib.instruments.bonds

import org.jquantlib.QL
import org.jquantlib.cashflow.CmtLeg
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.indexes.BondYieldIndex
import org.jquantlib.instruments.bonds.CmtRateBond
import org.jquantlib.math.matrixutilities.{Array => qlArray}
import org.jquantlib.time.{BusinessDayConvention, Date => qlDate, Schedule => qlSchedule}
import org.jquantlib.currencies.Currency
import squantlib.database.DB
import java.util.{Date => JavaDate}
	import org.jquantlib.time.Frequency
	import org.jquantlib.termstructures.Compounding

class JGBFloatBond(
		settlementDays:Int,
        faceAmount:Double,
        schedule:qlSchedule,
        index:BondYieldIndex,
        paymentDayCounter:DayCounter,
        paymentConvention:BusinessDayConvention,
        fixingDays:Int,
        gearings:qlArray,
        spreads:qlArray,
        caps:qlArray,
        floors:qlArray,
        inArrears:Boolean,
        redemption:Double,
        issueDate:qlDate,
        id:String, 
        currency:Currency,
        creditSpreadID:String,
        initialFX:Double)
        
        extends CmtRateBond(
			settlementDays,
	        faceAmount,
	        schedule,
	        index,
	        paymentDayCounter,
	        paymentConvention,
	        fixingDays,
	        gearings,
	        spreads,
	        caps,
	        floors,
	        inArrears,
	        redemption,
	        issueDate,
	        id, 
	        currency,
	        creditSpreadID,
	        initialFX){
	
	val coupons = DB.getCoupons(id)
	val refindex = "CMT10"
	
	override def accruedAmount():Double = accruedAmount(settlementDate)
	override def accruedAmount(settlement:qlDate):Double = 
	  coupons.map(c => c.accruedCoupon(settlement.longDate)).collect{case Some(r) => r}.sum * 100

	
	implicit def qlDateToJavaDate(d:qlDate):JavaDate = d.longDate
	
	override def `yield`(dc:DayCounter, comp:Compounding, freq:Frequency, accuracy:Double, maxEvaluations:Int):Double =
	  coupons.withFilter(_.isActive(settlementDate)).map(c => c.fixedrate).collect{case Some(r) => r}.sum

    override def `yield`(dc:DayCounter, comp:Compounding, freq:Frequency):Double = `yield`(dc, comp, freq, 1.0e-8, 100)
    
    override def `yield`(cleanPrice:Double, dc:DayCounter, comp:Compounding, freq:Frequency, settlementDate:qlDate, accuracy:Double, maxEvaluations:Int):Double = 
	    throw new java.lang.UnsupportedOperationException("Yield with price not supported for JGBR")

    override def `yield`(cleanPrice:Double, dc:DayCounter, comp:Compounding, freq:Frequency):Double =
	    throw new java.lang.UnsupportedOperationException("Yield with price not supported for JGBR")

    override def `yield`(cleanPrice:Double, dc:DayCounter, comp:Compounding, freq:Frequency, settlementDate:qlDate) =
	    throw new java.lang.UnsupportedOperationException("Yield with price not supported for JGBR")

    override def `yield`(cleanPrice:Double, dc:DayCounter, comp:Compounding, freq:Frequency, settlementDate:qlDate, accuracy:Double) =
	    throw new java.lang.UnsupportedOperationException("Yield with price not supported for JGBR")
	  
}
