package net.squantlib.schedule

import net.squantlib.util.{Date, FixingInformation, UnderlyingFixing}

case class KnockInCondition(
  trigger: UnderlyingFixing,
  refStart: Date,
  refEnd: Date,
  closeOnly: Boolean,
  triggerOnEqual: Boolean
){
  val isEmpty = trigger.isEmpty
}

object KnockInCondition {

  def empty = KnockInCondition(
    trigger = UnderlyingFixing.empty,
    refStart = Date.currentDate,
    refEnd = Date.currentDate,
    closeOnly = false,
    triggerOnEqual = true
  )


}
