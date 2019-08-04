package net.squantlib.schedule.call

import scala.collection.immutable.ListMap
import net.squantlib.util.DisplayUtils._
import net.squantlib.schedule.FixingLeg
import net.squantlib.util.FixingInformation

case class CallOption(
  triggerUp: Boolean,
  forward: Map[String, BigDecimal],
  forwardInputString: Map[String, String],
  bonus: Double,
  invertedTrigger:Boolean,
  invertedForward:Boolean,
  removeSatisfiedTriggers:Boolean
) {
  
//  def toMap:Map[String, Any] = if (invertedStrike) Map("inverted_strike" -> 1) else Map.empty

}

object CallOption {
  def empty = CallOption(true, Map.empty, Map.empty, 0.0, false, false, false)
}
