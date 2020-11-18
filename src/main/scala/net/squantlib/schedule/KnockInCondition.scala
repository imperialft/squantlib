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
  triggerOnAny:Boolean
)(implicit val fixingInformation:FixingInformation){
  val isEmpty = trigger.isEmpty

  private var knockInDateFixing:Option[(Date, UnderlyingFixing)] = null //getKnockInDateFixing

  def getKnockInDate:Option[Date] = getKnockInDateFixing.collect{case (d, f) => d}

  def getKnockInFixing:Option[UnderlyingFixing] = getKnockInDateFixing.collect{case (d, f) => f}

  def getKnockInDateFixing:Option[(Date, UnderlyingFixing)] = {
    if (knockInDateFixing == null) {
      knockInDateFixing = {
        if (isEmpty) None
        else computeKnockInDateFixing
      }
    }
    knockInDateFixing
  }

  def computeKnockInDateFixing():Option[(Date, UnderlyingFixing)] = {
    val historicalPrices:Map[Date, UnderlyingFixing] = {
      val closeFixings = DB.getHistoricalUnderlyingFixings(trigger.keySet, refStart, refEnd)

      if (closeOnly) closeFixings
      else if (triggerDown) closeFixings ++ DB.getHistoricalLowUnderlyingFixings(trigger.keySet, refStart, refEnd)
      else closeFixings ++ DB.getHistoricalHighUnderlyingFixings(trigger.keySet, refStart, refEnd)
    }

    if (historicalPrices.isEmpty) None
    else {
      val firstTriggeredDate:Option[(Date, UnderlyingFixing)] = historicalPrices.filter{case (d, hp) => isKnockedInPrice(hp, trigger)} match {
        case dhp if dhp.isEmpty => None
        case dhp => Some(dhp.minBy(_._1))
      }

      (firstTriggeredDate, historicalPrices.get(refEnd)) match {
        case (Some(r), Some(hsLast)) =>
          if (finalTrigger.isEmpty || isKnockedInPrice(hsLast, finalTrigger)) Some(r)
          else None
        case (Some(r), None) => Some(r)
        case _ => None
      }
    }
  }

    def isKnockedIn():Boolean = getKnockInDateFixing.isDefined


//  def isKnockedIn():Boolean = {
//    val historicalPrices:Map[Date, UnderlyingFixing] = {
//      val closeFixings = DB.getHistoricalUnderlyingFixings(trigger.keySet, refStart, refEnd)
//
//      if (closeOnly) closeFixings
//      else if (triggerDown) closeFixings ++ DB.getHistoricalLowUnderlyingFixings(trigger.keySet, refStart, refEnd)
//      else closeFixings ++ DB.getHistoricalHighUnderlyingFixings(trigger.keySet, refStart, refEnd)
//    }
//
//    if (historicalPrices.isEmpty) false
//    else historicalPrices.get(refEnd) match {
//      case Some(hsLast) =>
//        historicalPrices.values.exists(hp => isKnockedInPrice(hp, trigger)) && (finalTrigger.isEmpty || isKnockedInPrice(hsLast, finalTrigger))
//      case None =>
//        historicalPrices.values.exists(hp => isKnockedInPrice(hp, trigger))
//    }
//  }

  private def isKnockedInPrice(p:BigDecimal, trig:BigDecimal):Boolean = {
    (triggerOnEqual, triggerDown) match {
      case (true, false) => p >= trig
      case (false, false) => p > trig
      case (true, true) => p <= trig
      case (false, true) => p < trig
    }
  }

  private def isKnockedInPrice(p:UnderlyingFixing, trig:UnderlyingFixing):Boolean = {
    if (triggerOnAny) {
      trig.getDecimalValue.exists{case (ul, t) => p.getDecimalValue.get(ul).collect{case f => isKnockedInPrice(f, t)}.getOrElse(false)}
    } else {
      trig.getDecimalValue.forall{case (ul, t) => p.getDecimalValue.get(ul).collect{case f => isKnockedInPrice(f, t)}.getOrElse(false)}
    }
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
    triggerOnEqual = true,
    triggerOnAny = true
  )(FixingInformation.empty("JPY", "JPY"))


}
