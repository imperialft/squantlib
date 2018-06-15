package net.squantlib.schedule.call

import scala.collection.immutable.ListMap
import net.squantlib.util.DisplayUtils._
import net.squantlib.schedule.FixingLeg
import net.squantlib.util.FixingInformation

case class Callability(
    bermudan: Boolean,
    triggers: Map[String, Double],
    triggerUp: Boolean,
    targetRedemption: Option[Double],
    forward: Map[String, Double],
    bonusAmount: Double,
    removeSatisfiedTriggers: Boolean,
    inputString: Map[String, Any],
    var accumulatedPayments: Option[Double],
    var simulatedFrontier: Map[String, Double] = Map.empty
  )(implicit val fixingInfo: FixingInformation) extends FixingLeg {

  val triggerVariables = triggers.keySet

  val forwardVariables = forward.keySet

  def optionalTriggers:Option[Map[String, Double]] = {
    if (isFixed) {
      if (isTriggered(getFixings)) Some(triggers.map{case (k, v) => (k, 0.0)})
      else None
    }
    else if (isTrigger) Some(triggers)
    else None
  }

  val optionalForwardStrikes = {
    if (forward.isEmpty) None
    else Some(forward)
  }

  override val variables = triggerVariables ++ forwardVariables
  
  def triggerInputString:Map[String, String] = inputString.get("trigger") match {
    case Some(trig:Map[_, _]) => trig.map{
        case (k:String, v:String) => (k, v)
        case (k, v) => (k.toString, v.toString)
      }.toMap
    case _ => Map.empty
  }
  
  def isBermuda:Boolean = bermudan 
  
  def underlyings:List[String] = triggers.keys.toList ++ forward.keys.filter(k => !triggers.keySet.contains(k))
  
  def triggerValues(underlyings:List[String]):List[Option[Double]] = underlyings.map(triggers.get)
  
  def isTrigger:Boolean = !triggers.isEmpty
  
  def isForward:Boolean = !forward.isEmpty
  
  def isTargetRedemption:Boolean = targetRedemption.isDefined
  
  override def isFixed = isFixedTrigger || isFixedTargetRedemption || isEmpty
  
  def isFixedTrigger = isTrigger && (variables.isEmpty || (!preFixings.isEmpty && !isFutureFixing))
  
  def isFixedTargetRedemption:Boolean = isTargetRedemption && accumulatedPayments.isDefined && targetRedemption.isDefined && !isFutureFixing
  
  def isPriceable:Boolean = !triggers.values.exists(v => v.isNaN || v.isInfinity) &&
    !forward.values.exists(v => v.isNaN || v.isInfinity || v <= 0.0000000000001)
  // && forward.keySet.subsetOf(triggers.keySet)
  
  def isEmpty:Boolean = !bermudan && triggers.isEmpty && targetRedemption.isEmpty
  
  def fixedTriggerByTrigger:Option[Boolean] = 
    if (isFixed && isTrigger)
      Some(triggers.forall{case (k, v) => if (triggerUp) v <= getFixings(k) else v >= getFixings(k)})
    else None
  
  def fixedTriggerByTargetRedemption:Option[Boolean] = (targetRedemption, accumulatedPayments) match {
    case (Some(tgt), Some(acc)) => Some(acc >= tgt - 0.0000000001)
    case _ => None
  }
  
  def fixedTrigger:Option[Boolean] = fixedTriggerByTrigger match {
    case Some(t) => Some(t)
    case None => fixedTriggerByTargetRedemption
  }

  def judgeTrigger(f:Map[String, Double]):Boolean = 
    isTrigger && !triggers.isEmpty && (triggers.keySet subsetOf f.keySet) && triggers.forall{case (k, v) => if (triggerUp) v <= f(k) else v >= f(k)}
  
  def isTriggered(f:Map[String, Double]):Boolean = judgeTrigger(if (isFixed) getFixings else f)
  
  def isTriggered:Boolean = if (isFixed) isTriggered(getFixings) else false

//  def isTargetRedeemed:Boolean = (accumulatedPayments, targetRedemption) match {
//    case (Some(acc), Some(tgt)) => acc >= tgt
//    case _ => false
//  }

  def isProbablyCalled(f:Map[String, Double]):Boolean = 
    isTriggered(f) || 
    fixedTriggerByTargetRedemption.getOrElse(false) ||
    (
      bermudan && 
      !simulatedFrontier.isEmpty && 
      (simulatedFrontier.keySet subsetOf f.keySet) && 
      simulatedFrontier.forall{case (k, v) => if (triggerUp) v <= f(k) else v >= f(k)}
    )
    
  def isProbablyCalled:Boolean = if (isFixed) isProbablyCalled(getFixings) else false
     
//  def redemptionAmount:Double = 1.0 + bonus 
  
  def redemptionNominal = 1.0 + bonusAmount

  def redemptionAmount(f:Map[String, Double]):Double = {
    if (forward.isEmpty) redemptionNominal
    else if (forward.keySet.subsetOf(f.keySet)) forward.map {
      case (ul, fk) => redemptionNominal * f(ul) / fk
    }.min
    else Double.NaN
  }
    
  def fixedRedemptionAmount:Option[Double] = if (isFixed && isProbablyCalled) Some(redemptionAmount(getFixings)) else None
  
  val fixedRedemptionAmountAtTrigger:Double = {
    if (isForward) {
      if (triggers.size == forward.size && forward.keySet.subsetOf(triggers.keySet)) {
        redemptionNominal * forward.map { case (k, v) => triggers(k) / v }.min
      } else {
        Double.NaN
      }
    }
    else redemptionNominal
  }

  def triggerShifted(shiftedTriggers: Map[String, Double]):Callability = {
    Callability(
      bermudan = bermudan,
      triggers = shiftedTriggers,
      triggerUp = triggerUp,
      targetRedemption = targetRedemption,
      forward = forward,
      bonusAmount = bonusAmount,
      removeSatisfiedTriggers = removeSatisfiedTriggers,
      inputString = inputString,
      accumulatedPayments = accumulatedPayments,
      simulatedFrontier = simulatedFrontier
    )
  }


  //  trigger:List[Option[Map[String, Double]]],
//  trigUp: List[Boolean],
//  trigAmount:List[Double],
//  forwardStrikes: List[Option[Map[String, Double]]],
//  targets:List[Option[Double]],

  
  override def toString:String = 
    List(
	    if (bermudan) "call " else "",
	    if (isTrigger) (triggers.map{case (k, v) => k + ":" + v.asDouble}.mkString(" ")) else "",
	    if (isTrigger) {if (triggerUp) "up" else "down"} else "",
	    targetRedemption.collect{case t => "target : " + t.asPercent}.getOrElse(""),
	    if (bonusAmount != 0.0) "bonus " + bonusAmount.asPercent(3) else "",
	    if (forward.isEmpty) "" else "forward " + forward.map{case (k, v) => k + ":" + v.asDouble}.mkString(" "),
	    if (isEmpty) "no call" else "",
      if (removeSatisfiedTriggers) "memory" else ""
	    ).mkString(" ") 


}

object Callability {
  
  val empty = Callability(
    bermudan = false,
    triggers = ListMap.empty,
    targetRedemption = None,
    callOption = CallOption.empty,
    inputString = Map.empty,
    accumulatedPayments = None,
    simulatedFrontier= Map.empty
  )(FixingInformation.empty("JPY", "JPY"))

  def apply(
    bermudan:Boolean,
    triggers:Map[String, Double],
    targetRedemption:Option[Double],
    callOption: CallOption,
    inputString:Map[String, Any],
    accumulatedPayments:Option[Double],
    simulatedFrontier:Map[String, Double]
  )(implicit fixingInfo:FixingInformation):Callability = 
    Callability(
      bermudan = bermudan,
      triggers = triggers,
      triggerUp = callOption.triggerUp,
      targetRedemption = targetRedemption,
      forward = callOption.forward,
      bonusAmount = callOption.bonus,
      removeSatisfiedTriggers = callOption.removeSatisfiedTriggers,
      inputString = inputString,
      accumulatedPayments = accumulatedPayments,
      simulatedFrontier
    )

  def apply(
    underlyings:List[String],
    bermudan:Boolean, 
    triggers:List[Option[Double]],
    triggerUp:Boolean,
    targetRedemption:Option[Double],
    forward: Map[String, Double],
    bonusAmount:Double,
    removeSatisfiedTriggers: Boolean,
    inputString:Map[String, Any],
    accumulatedPayments:Option[Double],
    simulatedFrontier:Map[String, Double]
    )(implicit fixingInfo:FixingInformation):Callability = 
    Callability(
      bermudan = bermudan,
      triggers = underlyings.zip(triggers).collect{case (k, Some(v)) => (k, v)}.toMap,
      triggerUp = triggerUp,
      targetRedemption = targetRedemption,
      forward = forward,
      bonusAmount = bonusAmount,
      removeSatisfiedTriggers = removeSatisfiedTriggers,
      inputString = inputString,
      accumulatedPayments = accumulatedPayments,
      simulatedFrontier
    )
  
  
}

