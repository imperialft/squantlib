package squantlib.instruments.bonds

import org.jquantlib.QL
import org.jquantlib.cashflow.CmtLeg
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.indexes.BondYieldIndex
import org.jquantlib.instruments.{Bond => qlBond}
import org.jquantlib.math.matrixutilities.{Array => qlArray}
import org.jquantlib.time.{BusinessDayConvention, Date => qlDate, Schedule => qlSchedule}
import org.jquantlib.currencies.Currency


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
        
        extends qlBond(
            settlementDays, 
            schedule.calendar(), 
            issueDate, 
            id, 
            currency, 
            creditSpreadID, 
            initialFX)
		{
   
		maturityDate_ = schedule.endDate().clone
		cashflows_ = new CmtLeg(schedule, index)
				.withNotionals(faceAmount)
				.withPaymentDayCounter(paymentDayCounter)
				.withPaymentAdjustment(paymentConvention)
				.withFixingDays(fixingDays)
				.withGearings(gearings)
				.withSpreads(spreads)
				.withCaps(caps)
				.withFloors(floors) 
				.inArrears(inArrears).Leg

		addRedemptionsToCashflows(Array[Double](redemption))

		QL.ensure(!cashflows().isEmpty(), "bond with no cashflows!")
		QL.ensure(redemptions_.size() == 1, "multiple redemptions created")
		index.addObserver(this)
		
		def this(settlementDays:Int,
	            faceAmount:Double,
	            schedule:qlSchedule,
	            index:BondYieldIndex,
	            paymentDayCounter:DayCounter,
	            id:String, 
	            currency:Currency,
	            creditSpreadID:String,
	            initialFX:Double) {
			
			this(settlementDays, faceAmount, schedule, index, paymentDayCounter,
					BusinessDayConvention.Following, //default
					0,								//default fixing days
					new qlArray(1).fill(1.0),			//default gearings
					new qlArray(1).fill(0.0), 		//default spread
					new qlArray(0), 					//default caps
					new qlArray(0), 					//default floor
					false, 							//defaul in Arrears
					100.0,							// default redemption
					new qlDate(), 					// default issue date
					id,
					currency,
					creditSpreadID,
					initialFX
					);
		}
}

