package net.squantlib.schedule.call

import scala.collection.immutable.ListMap
import net.squantlib.util.DisplayUtils._
import net.squantlib.schedule.FixingLeg
import net.squantlib.util.FixingInformation

case class CallOption(
  triggerUp: Boolean,
  forward: Map[String, Double],
  bonus: Double
)

object CallOption {
  def empty = CallOption(true, Map.empty, 0.0)
}
