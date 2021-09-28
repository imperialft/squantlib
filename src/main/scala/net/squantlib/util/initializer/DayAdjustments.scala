package net.squantlib.util.initializer

import net.squantlib.util.ql.time.BusinessDayConvention
import BusinessDayConvention._ 

object DayAdjustments extends Initializer[BusinessDayConvention] {
  
	val mapper = Map(
		("UNADJUSTED" -> Unadjusted),
		("FOLLOWING" -> Following),
		("M_FOLLOWING" -> ModifiedFollowing),
		("PRECEDING" -> Preceding),
		("M_PRECEDING" -> ModifiedPreceding))
			
}