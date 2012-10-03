package squantlib.setting.initializer

import org.jquantlib.time.BusinessDayConvention

object DayAdjustments extends Initializer[BusinessDayConvention] {
  
	import BusinessDayConvention._ 
	val mapper = Map(
		("UNADJUSTED" -> Unadjusted),
		("FOLLOWING" -> Following),
		("M_FOLLOWING" -> ModifiedFollowing),
		("PRECEDING" -> Preceding),
		("M_PRECEDING" -> ModifiedPreceding))
			
}