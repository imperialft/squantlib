package net.squantlib.model.yieldparameter

import org.jquantlib.time.{Period => qlPeriod}
import net.squantlib.util.Date
import scala.collection.Map

/**
 * Flat vector
 * @param input point
 */
case class FlatVector(var valuedate : Date, value:Double) extends YieldParameter with AbstractYieldParameter {
	
	override val mindays = 0.0
	override val maxdays = 0.0

	override def lowextrapolation(v : Double) = value
    override def highextrapolation(v : Double) = value
    override def interpolation(v : Double) = value
    
    override def shifted(shift:(Double, Double) => Double):FlatVector = new FlatVector(valuedate, shift(0.0, value))
}


object FlatVector {
  
	def apply(valuedate : Date, inputvalues:Map[qlPeriod, Double]):FlatVector = {
	  require(inputvalues.size == 1, "flat curve can have 1 point only : found " + inputvalues.size)
	  FlatVector(valuedate, inputvalues.head._2)
	}

	def apply(valuedate : Date, inputvalues: => Map[Double, Double]):FlatVector = {
	  require(inputvalues.size == 1, "flat curve can have 1 point only : found " + inputvalues.size)
	  FlatVector(valuedate, inputvalues.head._2)
	}

}