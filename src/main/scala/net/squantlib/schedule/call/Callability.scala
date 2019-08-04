package net.squantlib.schedule.call

import scala.collection.immutable.ListMap
import net.squantlib.util.DisplayUtils._
import net.squantlib.schedule.FixingLeg
import net.squantlib.util.FixingInformation
import net.squantlib.schedule.payoff._
import net.squantlib.util.JsonUtils

case class Callability(
  bermudan: Boolean,
  triggers: Map[String, BigDecimal],
  triggerUp: Boolean,
  targetRedemption: Option[BigDecimal],
  forward: Map[String, BigDecimal],
  bonusAmount: BigDecimal,
  removeSatisfiedTriggers: Boolean,
  inputString: Map[String, Any],
  var accumulatedPayments: Option[Double],
  var simulatedFrontier: Map[String, Double] = Map.empty
)(implicit val fixingInfo: FixingInformation) extends FixingLeg {

  val triggerVariables = triggers.keySet

  val forwardVariables = forward.keySet

  val doubleForward = forward.map{case (ul, v) => (ul, v.toDouble)}

  var issuerCalled:Option[Boolean] = None

  def setIssuerCalled(setTrue:Boolean = true) = {
    issuerCalled = Some(setTrue)
  }

  def optionalTriggers:Option[Map[String, BigDecimal]] = {
    if (isFixed) {
      if (isTriggered(getFixings)) Some(triggers.map{case (k, v) => (k, BigDecimal(0.0))})
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
    }
    case _ => Map.empty
  }
  
  def isBermuda:Boolean = bermudan 
  
  def underlyings:List[String] = triggers.keys.toList ++ forward.keys.filter(k => !triggers.keySet.contains(k))
  
  def triggerValues(underlyings:List[String]):List[Option[BigDecimal]] = underlyings.map(ul => triggers.get(ul))
  
  def isTrigger:Boolean = !triggers.isEmpty
  
  def isForward:Boolean = !forward.isEmpty
  
  def isTargetRedemption:Boolean = targetRedemption.isDefined
  
  override def isFixed = isFixedTrigger || isFixedTargetRedemption || isEmpty
  
  def isFixedTrigger = isTrigger && (variables.isEmpty || (!preFixings.isEmpty && !isFutureFixing))
  
  def isFixedTargetRedemption:Boolean = isTargetRedemption && accumulatedPayments.isDefined && targetRedemption.isDefined && !isFutureFixing
  
  def isPriceable:Boolean = !forward.values.exists(v => v <= 0.0000000000001)
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

  def fixedTrigger:Option[Boolean] = {
    if (issuerCalled == Some(true)) Some(true)
    else fixedTriggerByTrigger match {
      case Some(t) => Some(t)
      case None => fixedTriggerByTargetRedemption
    }
  }

  def judgeTriggerDouble(f:Map[String, Double]):Boolean = (
    isTrigger && !triggers.isEmpty && (triggers.keySet subsetOf f.keySet) &&
    triggers.forall{case (ul, v) =>
      if (triggerUp) v ~<= (f(ul), ul)
      else v ~>= (f(ul), ul)
    }
  )

  def isTriggeredDouble(f:Map[String, Double]):Boolean = judgeTriggerDouble(if (isFixed) getDoubleFixings else f)

  def judgeTrigger(f:Map[String, BigDecimal]):Boolean = (
    isTrigger && !triggers.isEmpty && (triggers.keySet subsetOf f.keySet) &&
      triggers.forall{case (ul, v) =>
        if (triggerUp) v <= f(ul)
        else v >= f(ul)
      }
    )

  def isTriggered(f:Map[String, BigDecimal]):Boolean = judgeTrigger(if (isFixed) getFixings else f)

  def isTriggered:Boolean = {
    if (isFixed) isTriggered(getFixings)
    else false
  }

//  def isTargetRedeemed:Boolean = (accumulatedPayments, targetRedemption) match {
//    case (Some(acc), Some(tgt)) => acc >= tgt
//    case _ => false
//  }

  def isProbablyCalled(f:Map[String, Double]):Boolean = 
    isTriggeredDouble(f) ||
    fixedTriggerByTargetRedemption.getOrElse(false) ||
    (
      bermudan && 
      !simulatedFrontier.isEmpty && 
      (simulatedFrontier.keySet subsetOf f.keySet) && 
      simulatedFrontier.forall{case (k, v) => if (triggerUp) v <= f(k) else v >= f(k)}
    )
    
  def isProbablyCalled:Boolean = if (isFixed) isProbablyCalled(getDoubleFixings) else false
     
//  def redemptionAmount:Double = 1.0 + bonus 
  
  def redemptionNominal:BigDecimal = 1.0 + bonusAmount

  def redemptionAmount(f:Map[String, Double]):Double = {
    if (forward.isEmpty) redemptionNominal.toDouble
    else if (forward.keySet.subsetOf(f.keySet)) doubleForward.map {
      case (ul, fk) => (redemptionNominal.toDouble * f(ul) / fk.toDouble)
    }.min
    else Double.NaN
  }

  lazy val redemptionPayoff:Payoff = {
    if (forward.isEmpty) FixedPayoff(redemptionNominal)
    else {
      val fwdVarList = forward.keys.toList
      val jsonString = JsonUtils.jsonString(Map(
        "type" -> "forward",
        "variable" -> forward.keySet,
        "strike" -> inputString.getOrElse("forward", forward)
      ))
      new ForwardPayoff(
        strikes = forward,
        physical = true,
        reverse = false,
        minPayoff = 0.0,
        maxPayoff = None,
        description = "",
        inputString = jsonString
      )
    }
  }
    
  def fixedRedemptionAmount:Option[Double] = if (isFixed && isProbablyCalled) Some(redemptionAmount(getDoubleFixings)) else None
  
  val fixedRedemptionAmountAtTrigger:Double = {
    if (isForward) {
      if (triggers.size == forward.size && forward.keySet.subsetOf(triggers.keySet)) {
        redemptionNominal.toDouble * forward.map { case (k, v) => (triggers(k).toDouble / v.toDouble) }.min
      } else {
        Double.NaN
      }
    }
    else redemptionNominal.toDouble
  }

  def triggerShifted(shiftedTriggers: Map[String, Double]):Callability = {
    Callability(
      bermudan = bermudan,
      triggers = shiftedTriggers.getDecimal,
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
    triggers = Map.empty[String, BigDecimal],
    targetRedemption = None,
    callOption = CallOption.empty,
    inputString = Map.empty[String, Any],
    accumulatedPayments = None,
    simulatedFrontier= Map.empty[String, Double]
  )(FixingInformation.empty("JPY", "JPY"))

  def apply(
    bermudan:Boolean,
    triggers:Map[String, BigDecimal],
    targetRedemption:Option[BigDecimal],
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
    forward: Map[String, BigDecimal],
    bonusAmount:Double,
    removeSatisfiedTriggers: Boolean,
    inputString:Map[String, Any],
    accumulatedPayments:Option[Double],
    simulatedFrontier:Map[String, Double]
    )(implicit fixingInfo:FixingInformation):Callability = 
    Callability(
      bermudan = bermudan,
      triggers = underlyings.zip(triggers).collect{case (k, Some(v)) => (k, v.getDecimal(k))}.collect{case (k, Some(v)) => (k, v)}.toMap,
      triggerUp = triggerUp,
      targetRedemption = targetRedemption.collect{case v => BigDecimal.valueOf(v)},
      forward = forward,
      bonusAmount = BigDecimal.valueOf(bonusAmount),
      removeSatisfiedTriggers = removeSatisfiedTriggers,
      inputString = inputString,
      accumulatedPayments = accumulatedPayments,
      simulatedFrontier
    )
  
  
}

