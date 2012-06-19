package squantlib.parameter.yieldparameter

import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }
import org.jquantlib.time.TimeUnit


/**
 * Flat vector
 * @param input point
 */
class FlatVector(var valuedate : JDate, inputvalues:Map[JPeriod, Double]) extends YieldParameter with AbstractYieldParameter {
	require(inputvalues.size == 1)
	
	val constantvalue = inputvalues.first._2
	val firstvalue = inputvalues.first._2
	
	val mindays = inputvalues.first._1.days(valuedate)
	val maxdays = mindays
	val maxdate = new JDate(valuedate.serialNumber() + maxdays)
	val maxperiod = new JPeriod(maxdays.toInt, TimeUnit.Days)

	def lowextrapolation(v : Long) = constantvalue
    def highextrapolation(v : Long) = constantvalue
    def interpolation(v : Long) = constantvalue
}

