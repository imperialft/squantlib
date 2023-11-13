package net.squantlib.schedule.call

import net.squantlib.schedule.{KnockInCondition}
import net.squantlib.util.{Date, UnderlyingFixing, FixingInformation}

case class TriggerCondition(
  private val strikes: UnderlyingFixing,
  triggerUp: Boolean,
  removeSatisfiedTriggers: Boolean,
  resetCondition: KnockInCondition,
  resetStrikes: UnderlyingFixing,
  var isReset:Boolean,
  barrierCondition: KnockInCondition,
  redemptionFrom: Option[Date],
  redemptionAfter: Option[Int],
  fullCouponOnBarrier: Boolean
){

  def getStrikes:UnderlyingFixing = if (isReset) resetStrikes else strikes

  def isActive:Boolean = !getStrikes.isEmpty

  def isTriggered(f:UnderlyingFixing):Boolean = {
    isActive && !getStrikes.isEmpty && getStrikes.isAllValid && f.isValidFor(getStrikes.keySet) &&
      getStrikes.getDecimal.forall { case (ul, v) =>
        if (triggerUp) v <= f.getDecimal(ul)
        else v >= f.getDecimal(ul)
      }
  }

  def strikeShifted(newStrikes:UnderlyingFixing):TriggerCondition = TriggerCondition(
    strikes = newStrikes,
    triggerUp = triggerUp,
    removeSatisfiedTriggers = removeSatisfiedTriggers,
    resetCondition = resetCondition,
    resetStrikes = resetStrikes,
    isReset = isReset,
    barrierCondition = barrierCondition,
    redemptionFrom = redemptionFrom,
    redemptionAfter = redemptionAfter,
    fullCouponOnBarrier = fullCouponOnBarrier
  )

}

object TriggerCondition {

  def empty = TriggerCondition(
    strikes = UnderlyingFixing.empty,
    triggerUp = true,
    removeSatisfiedTriggers = false,
    resetCondition = KnockInCondition.empty,
    resetStrikes = UnderlyingFixing.empty,
    isReset = false,
    barrierCondition = KnockInCondition.empty,
    redemptionFrom = None,
    redemptionAfter = None,
    fullCouponOnBarrier = true
  )

  def initialize(
    strikes: UnderlyingFixing,
    triggerUp: Boolean,
    removeSatisfiedTriggers: Boolean,
    resetCondition: KnockInCondition,
    resetStrikes: UnderlyingFixing,
    barrierCondition: KnockInCondition,
    redemptionFrom: Option[Date],
    redemptionAfter: Option[Int],
    fullCouponOnBarrier: Boolean
  )(implicit fixingInformation:FixingInformation):TriggerCondition = {
    val isReset:Boolean = {
      if (resetCondition.isEmpty) false
      else resetCondition.isKnockedIn
    }

    TriggerCondition(
      strikes = strikes,
      triggerUp = triggerUp,
      removeSatisfiedTriggers = removeSatisfiedTriggers,
      resetCondition = resetCondition,
      resetStrikes = resetStrikes,
      isReset = isReset,
      barrierCondition = barrierCondition,
      redemptionFrom = redemptionFrom,
      redemptionAfter = redemptionAfter,
      fullCouponOnBarrier = fullCouponOnBarrier
    )
  }

}
