package squantlib.model.yieldparameter

import org.jquantlib.time.{ TimeUnit, Period => JPeriod, Date => JDate }
import scala.collection.Map

/**
 * Flat vector
 * @param input point
 */
class FlatVector(var valuedate : JDate, inputvalues:Map[JPeriod, Double]) extends YieldParameter with AbstractYieldParameter {
	require(inputvalues.size == 1, "flat curve can have 1 point only : found " + inputvalues.size)
	
	val constantvalue = inputvalues.first._2
	val firstvalue = inputvalues.first._2
	
	val mindays = inputvalues.first._1.days(valuedate).toDouble
	val maxdays = mindays

	def lowextrapolation(v : Double) = constantvalue
    def highextrapolation(v : Double) = constantvalue
    def interpolation(v : Double) = constantvalue
    
    def shifted(shift:(Double, Double) => Double):FlatVector = new FlatVector(valuedate, inputvalues.map{case (k, v) => (k, shift(k.days(valuedate).toDouble, v))}.toMap)
    
    def this(valuedate:JDate, inputvalue:Double) = this(valuedate, Map(new JPeriod(1, TimeUnit.Months) -> inputvalue))
}

