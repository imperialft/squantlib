package net.squantlib.util.initializer

import org.jquantlib.time.BusinessDayConvention
import BusinessDayConvention._ 

object DayAdjustments extends Initializer[BusinessDayConvention] {
  
	val mapper = Map(
		("UNADJUSTED" -> Unadjusted),
		("FOLLOWING" -> Following),
		("M_FOLLOWING" -> ModifiedFollowing),
		("PRECEDING" -> Preceding),
		("M_PRECEDING" -> ModifiedPreceding))
			
}