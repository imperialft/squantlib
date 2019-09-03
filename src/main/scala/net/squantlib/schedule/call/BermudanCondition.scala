package net.squantlib.schedule.call

import net.squantlib.schedule.KnockInCondition
import net.squantlib.util.UnderlyingFixing

case class BermudanCondition(
  isActive: Boolean
){


}

object BermudanCondition {

  def empty = BermudanCondition(false)
}


