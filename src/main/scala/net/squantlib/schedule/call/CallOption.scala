package net.squantlib.schedule.call

import net.squantlib.util.{Date, FixingInformation, UnderlyingFixing}
import com.fasterxml.jackson.databind.JsonNode
import net.squantlib.util.UnderlyingFixing
import net.squantlib.util.JsonUtils._


case class CallOption(
  triggerUp: Boolean,
  forward: UnderlyingFixing,
  forwardInputString: Map[String, String],
  bonus: Double,
  invertedTrigger:Boolean,
  invertedForward:Boolean,
  barrierRedemptionAfter: Option[Int],
  fullCouponOnBarrier: Boolean,
  removeSatisfiedTriggers:Boolean,
  exercised:Option[Boolean]
) {


}

object CallOption {

  def empty = CallOption(
    triggerUp = true,
    forward = UnderlyingFixing.empty,
    forwardInputString = Map.empty,
    bonus = 0.0,
    invertedTrigger = false,
    invertedForward = false,
    barrierRedemptionAfter = None,
    fullCouponOnBarrier = true,
    removeSatisfiedTriggers = false,
    exercised = None
  )

  def parseJson(
    jsonNode:JsonNode
  ) (implicit fixingInfo:FixingInformation):CallOption = {
    val invertedStrike:Boolean = jsonNode.parseInt("inverted_strike").getOrElse(0) == 1
    val invertedTrigger:Boolean = (invertedStrike || jsonNode.parseInt("inverted_trigger").getOrElse(0) == 1)
    val invertedForward:Boolean = (invertedStrike || jsonNode.parseInt("inverted_forward").getOrElse(0) == 1)

    val barrierRedemptionAfter = jsonNode.parseInt("redemption_after") match {
      case Some(d) if d <= 0 => None
      case v => v
    }

    val triggerUp = jsonNode.parseString("trigger_type").getOrElse("up") == "up"
    val forwardMap = jsonNode.getOption("forward").collect{case k => k.parseStringFields}.getOrElse(Map.empty)
    val forward = fixingInfo.computeStrikes(forwardMap)
    val bonus = jsonNode.parseDouble("bonus").getOrElse(0.0)
    val removeSatisfiedTriggers = jsonNode.parseInt("memory").getOrElse(0) == 1
    val issuerExercised:Option[Boolean] = jsonNode.parseInt("exercised").collect{case v => v == 1}
    val forwardStrikes = {
      if (invertedForward && forward.keySet.forall(_.size == 6))
        UnderlyingFixing(forward.getDouble.map{case (k, v) => ((k takeRight 3) + (k take 3), if(v != 0.0) 1.0 / v else 0.0)})
      else forward
    }

    val fullCouponOnBarrier:Boolean = jsonNode.get("final_coupon").parseString.collect{case i => i != "accrued"}.getOrElse(true)

    CallOption(
      triggerUp = if (invertedTrigger) !triggerUp else triggerUp,
      forward = forwardStrikes,
      forwardInputString = forwardMap,
      bonus = bonus,
      invertedTrigger = invertedTrigger,
      invertedForward = invertedForward,
      barrierRedemptionAfter = barrierRedemptionAfter,
      fullCouponOnBarrier = fullCouponOnBarrier,
      removeSatisfiedTriggers = removeSatisfiedTriggers,
      exercised = issuerExercised
    )

  }

}
