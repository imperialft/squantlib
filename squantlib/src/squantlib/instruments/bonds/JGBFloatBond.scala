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
	
	override def accruedAmount(settlement:qlDate):Double = 
	  coupons.map(c => c.accruedCoupon(settlement.longDate)).collect{case Some(r) => r}.sum * 100

}
