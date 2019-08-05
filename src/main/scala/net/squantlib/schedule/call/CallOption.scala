package net.squantlib.schedule.call

import scala.collection.immutable.ListMap
import net.squantlib.util.DisplayUtils._
import net.squantlib.schedule.FixingLeg
import net.squantlib.util.FixingInformation

case class CallOption(
  triggerUp: Boolean,
  forwardDefinition: Map[String, Option[BigDecimal]],
  forwardInputString: Map[String, String],
  bonus: Double,
  invertedTrigger:Boolean,
  invertedForward:Boolean,
  removeSatisfiedTriggers:Boolean
) {
  

}

object CallOption {
  def empty = CallOption(true, Map.empty, Map.empty, 0.0, false, false, false)
}
