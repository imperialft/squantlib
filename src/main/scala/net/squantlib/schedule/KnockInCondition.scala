package net.squantlib.schedule

import net.squantlib.database.DB
import net.squantlib.util.{Date, FixingInformation, UnderlyingFixing}

case class KnockInCondition(
  trigger: UnderlyingFixing,
  refStart: Date,
  refEnd: Date,
  finalTrigger: UnderlyingFixing,
  closeOnly: Boolean,
  triggerDown:Boolean,
  triggerOnEqual: Boolean,
){
  val isEmpty = trigger.isEmpty

  def isKnockedIn()(implicit fixingInformation:FixingInformation):Boolean = {
    val historicalPrices:Map[Date, UnderlyingFixing] = {
      val closeFixings = DB.getHistoricalUnderlyingFixings(trigger.keySet, refStart, refEnd)

      if (closeOnly) closeFixings
      else if (triggerDown) closeFixings ++ DB.getHistoricalLowUnderlyingFixings(trigger.keySet, refStart, refEnd)
      else closeFixings ++ DB.getHistoricalHighUnderlyingFixings(trigger.keySet, refStart, refEnd)
    }

    if (historicalPrices.isEmpty) false
    else historicalPrices.get(refEnd) match {
      case Some(hsLast) =>
        historicalPrices.values.exists(hp => isKnockedInPrice(hp, trigger)) && (finalTrigger.isEmpty || isKnockedInPrice(hsLast, finalTrigger))
      case None =>
        historicalPrices.values.exists(hp => isKnockedInPrice(hp, trigger))
    }
  }

  private def isKnockedInPrice(p:BigDecimal, trig:BigDecimal):Boolean = {
    (triggerOnEqual, triggerDown) match {
      case (true, false) => p >= trig
      case (false, false) => p > trig
      case (true, true) => p <= trig
      case (false, true) => p < trig
    }
  }

  private def isKnockedInPrice(p:UnderlyingFixing, trig:UnderlyingFixing):Boolean = {
    trig.getDecimalValue.exists{case (ul, t) => p.getDecimalValue.get(ul).collect{case f => isKnockedInPrice(f, t)}.getOrElse(false)}
  }

}

object KnockInCondition {

  def empty = KnockInCondition(
    trigger = UnderlyingFixing.empty,
    refStart = Date.currentDate,
    refEnd = Date.currentDate,
    finalTrigger = UnderlyingFixing.empty,
    closeOnly = false,
    triggerDown = true,
    triggerOnEqual = true
  )


}
