package squantlib.database.objectconstructor

import org.jquantlib.daycounters._
import org.jquantlib.time.BusinessDayConvention

object DaycountConstructor {
  
  	def getdaycount_adj = Map(
			("UNADJUSTED" -> BusinessDayConvention.Unadjusted),
			("FOLLOWING" -> BusinessDayConvention.Following),
			("M_FOLLOWING" -> BusinessDayConvention.ModifiedFollowing),
			("PRECEDING" -> BusinessDayConvention.Preceding),
			("M_PRECEDING" -> BusinessDayConvention.ModifiedPreceding))
			
	def getdaycount = Map(
			("30/360" -> new Thirty360),
			("ACT/360" -> new Actual360),
			("ACT/365" -> new Actual365Fixed),
			("ACT/ACT" -> new ActualActual))

}