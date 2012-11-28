package squantlib.jquantlib.instruments.bonds

import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.{BusinessDayConvention, Date => qlDate, Schedule => qlSchedule, Frequency}
import org.jquantlib.currencies.Currency
import org.jquantlib.instruments.bonds.FixedRateBond
import org.jquantlib.termstructures.Compounding
import squantlib.database.DB
import java.util.{Date => JavaDate}

class JGBFixedBond(
		settlementDays:Int,
        faceAmount:Double,
        schedule:qlSchedule,
        fixcoupons:Array[Double],
        paymentDayCounter:DayCounter,
        paymentConvention:BusinessDayConvention,
        redemption:Double,
        issueDate:qlDate,
        id:String, 
        currency:Currency,
        creditSpreadID:String,
        initialFX:Double)
        
        extends FixedRateBond(
            settlementDays,
            faceAmount,
            schedule,
            fixcoupons,
            paymentDayCounter,
            paymentConvention,
            redemption,
            issueDate,
            id,
            currency,
            creditSpreadID,
            initialFX) {
  
	val coupons = DB.getCouponsByBondID(id)
	
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

