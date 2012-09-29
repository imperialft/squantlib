package squantlib.instruments.bonds

import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.{BusinessDayConvention, Date => qlDate, Schedule => qlSchedule}
import org.jquantlib.currencies.Currency
import org.jquantlib.instruments.bonds.FixedRateBond


class JGBFixedBond(
		settlementDays:Int,
        faceAmount:Double,
        schedule:qlSchedule,
        coupons:Array[Double],
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
            coupons,
            paymentDayCounter,
            paymentConvention,
            redemption,
            issueDate,
            id,
            currency,
            creditSpreadID,
            initialFX)

