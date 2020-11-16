package net.squantlib.schedule.call

import scala.collection.immutable.ListMap
import net.squantlib.util.UnderlyingFixing
import net.squantlib.schedule.FixingLeg
import net.squantlib.util.FixingInformation

case class CallOption(
  triggerUp: Boolean,
  dailyFixing:Boolean,
  dailyCloseOnly: Boolean,
  redemptionAfter: Option[Int],
  forward: UnderlyingFixing,
  forwardInputString: Map[String, String],
  bonus: Double,
  invertedTrigger:Boolean,
  invertedForward:Boolean,
  removeSatisfiedTriggers:Boolean,
  exercised:Option[Boolean]
) {


}

object CallOption {

  def empty = CallOption(
    triggerUp = true,
    dailyFixing = false,
    dailyCloseOnly = true,
    redemptionAfter = None,
    forward = UnderlyingFixing.empty,
    forwardInputString = Map.empty,
    bonus = 0.0,
    invertedTrigger = false,
    invertedForward = false,
    removeSatisfiedTriggers = false,
    exercised = None
  )

}
