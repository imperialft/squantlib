package net.squantlib.schedule.call

import scala.collection.immutable.ListMap
import net.squantlib.util.DisplayUtils._
import net.squantlib.schedule.{CalculationPeriod, FixingLeg}
import net.squantlib.util.{Date, FixingInformation, JsonUtils, UnderlyingFixing}
import net.squantlib.schedule.payoff._

case class Callability(
  bermudan: Boolean,
  triggers: UnderlyingFixing,
  triggerUp: Boolean,
  targetRedemption: Option[BigDecimal],
  forward: UnderlyingFixing,
  bonusAmount: BigDecimal,
  removeSatisfiedTriggers: Boolean,
  inputString: Map[String, Any],
  var accumulatedPayments: Option[Double],
  var simulatedFrontier: UnderlyingFixing = UnderlyingFixing.empty
)(implicit val fixingInfo: FixingInformation) extends FixingLeg {

  val triggerVariables = triggers.keySet

  val forwardVariables = forward.keySet

  var issuerCalled:Option[Boolean] = None

  def setIssuerCalled(setTrue:Boolean = true) = {
    issuerCalled = Some(setTrue)
  }

  def optionalTriggers:Option[UnderlyingFixing] = {
    if (isFixed) {
      if (isTriggered(getFixings)) Some(UnderlyingFixing(triggers.keySet.map(ul => (ul, Some(BigDecimal(0.0)))).toMap))
      else None
    }
    else if (isTrigger) Some(triggers)
    else None
  }

  val optionalForwardStrikes:Option[UnderlyingFixing] = {
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
  
  def underlyings:Set[String] = variables
  
  def triggerValues(underlyings:List[String]):List[Option[BigDecimal]] = underlyings.map(ul => triggers.getDecimalValue.get(ul))
  
  def isTrigger:Boolean = !triggers.isEmpty
  
  def isForward:Boolean = !forward.isEmpty
  
  def isTargetRedemption:Boolean = targetRedemption.isDefined
  
  override def isFixed = isFixedTrigger || isFixedTargetRedemption || isEmpty
  
  def isFixedTrigger = isTrigger && (variables.isEmpty || (!preFixings.isEmpty && !isFutureFixing))
  
  def isFixedTargetRedemption:Boolean = isTargetRedemption && accumulatedPayments.isDefined && targetRedemption.isDefined && !isFutureFixing
  
  def isPriceable:Boolean = forward.getDouble.values.forall(v => v > 0.0000000000001) && triggers.getDecimal.values.forall(_.isDefined) && forward.getDecimal.values.forall(_.isDefined)

  def isEmpty:Boolean = !bermudan && triggers.isEmpty && targetRedemption.isEmpty
  
  def fixedTriggerByTrigger:Option[Boolean] = 
    if (isFixed && isTrigger)
      Some(triggers.getDecimal.forall{case (k, v) => if (triggerUp) v <= getFixings.getDecimal(k) else v >= getFixings.getDecimal(k)})
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

  def eventDate(d:CalculationPeriod):List[Date] = {
    List(d.eventDate)
  }

//  def judgeTriggerDouble(f:Map[String, Double]):Boolean = (
//    isTrigger && !triggers.isEmpty && (triggers.keySet subsetOf f.keySet) &&
//    triggers.forall{case (ul, v) =>
//      if (triggerUp) v ~<= (f(ul), ul)
//      else v ~>= (f(ul), ul)
//    }
//  )
//

  def judgeTrigger(f:UnderlyingFixing):Boolean =
    isTrigger && !triggers.isEmpty && f.isValidFor(triggers.keySet) &&
    triggers.getDecimal.forall{case (ul, v) =>
      if (triggerUp) v <= f.getDecimal(ul)
      else v >= f.getDecimal(ul)
    }


  def isTriggered(f:Map[String, Double]):Boolean = isTriggered(UnderlyingFixing(f))

  def isTriggered(f:UnderlyingFixing):Boolean = judgeTrigger(if (isFixed) getFixings else f)

  def isTriggered:Boolean = {
    if (isFixed) isTriggered(getFixings)
    else false
  }

//  def isTargetRedeemed:Boolean = (accumulatedPayments, targetRedemption) match {
//    case (Some(acc), Some(tgt)) => acc >= tgt
//    case _ => false
//  }

  def isProbablyCalled(f:UnderlyingFixing):Boolean =
    isTriggered(f) ||
    fixedTriggerByTargetRedemption.getOrElse(false) ||
    (
      bermudan && 
      !simulatedFrontier.isEmpty && 
      f.isValidFor(simulatedFrontier.keySet) &&
      simulatedFrontier.getDecimal.forall{case (k, v) => if (triggerUp) v <= f.getDecimal(k) else v >= f.getDecimal(k)}
    )
    
  def isProbablyCalled:Boolean = if (isFixed) isProbablyCalled(getFixings) else false
     
//  def redemptionAmount:Double = 1.0 + bonus 
  
  def redemptionNominal:BigDecimal = 1.0 + bonusAmount

  def redemptionAmount(f:UnderlyingFixing):Double = {
    if (forward.isEmpty) redemptionNominal.toDouble
    else if (f.isValidFor(forward.keySet)) forward.getDouble.map {
      case (ul, fk) => (redemptionNominal.toDouble * f.getDouble(ul) / fk.toDouble)
    }.min
    else Double.NaN
  }

  lazy val redemptionPayoff:Payoff = {
    if (forward.isEmpty) FixedPayoff(redemptionNominal)
    else {
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
    
  def fixedRedemptionAmount:Option[Double] = if (isFixed && isProbablyCalled) Some(redemptionAmount(getFixings)) else None
  
  val fixedRedemptionAmountAtTrigger:Double = {
    if (isForward) {
      if (triggers.size == forward.size && triggers.isValidFor(forward.keySet)) {
        redemptionNominal.toDouble * forward.getDouble.map { case (k, v) => (triggers.getDouble(k) / v.toDouble) }.min
      } else {
        Double.NaN
      }
    }
    else redemptionNominal.toDouble
  }

  def triggerShifted(shiftedTriggers: Map[String, Double]):Callability = triggerShifted(UnderlyingFixing(shiftedTriggers))

  def triggerShifted(shiftedTriggers: UnderlyingFixing):Callability = {
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


  override def toString:String =
    List(
	    if (bermudan) "call " else "",
	    if (isTrigger) triggers else "",
	    if (isTrigger) {if (triggerUp) "up" else "down"} else "",
	    targetRedemption.collect{case t => "target : " + t.asPercent}.getOrElse(""),
	    if (bonusAmount != 0.0) "bonus " + bonusAmount.asPercent(3) else "",
	    if (forward.isEmpty) "" else "forward " + forward,
	    if (isEmpty) "no call" else "",
      if (removeSatisfiedTriggers) "memory" else ""
	    ).mkString(" ") 


}

object Callability {
  
  val empty = Callability(
    bermudan = false,
    triggers = UnderlyingFixing.empty,
    targetRedemption = None,
    callOption = CallOption.empty,
    inputString = Map.empty[String, Any],
    accumulatedPayments = None,
    simulatedFrontier= UnderlyingFixing.empty
  )(FixingInformation.empty("JPY", "JPY"))

  def apply(
    bermudan:Boolean,
    triggers: UnderlyingFixing,
    targetRedemption:Option[BigDecimal],
    callOption: CallOption,
    inputString:Map[String, Any],
    accumulatedPayments:Option[Double],
    simulatedFrontier:UnderlyingFixing
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
      simulatedFrontier = simulatedFrontier
    )

  def apply(
    underlyings:List[String],
    bermudan:Boolean, 
    triggers:List[Option[Double]],
    triggerUp:Boolean,
    targetRedemption:Option[Double],
    forward: UnderlyingFixing,
    bonusAmount:Double,
    removeSatisfiedTriggers: Boolean,
    inputString:Map[String, Any],
    accumulatedPayments:Option[Double],
    simulatedFrontier:UnderlyingFixing
    )(implicit fixingInfo:FixingInformation):Callability = 
    Callability(
      bermudan = bermudan,
      triggers = UnderlyingFixing(underlyings.zip(triggers).collect{case (k, Some(v)) => (k, v)}.toMap),
      triggerUp = triggerUp,
      targetRedemption = targetRedemption.flatMap{case v => v.getRoundedDecimal},
      forward = forward,
      bonusAmount = bonusAmount.getRoundedDecimal.getOrElse(0.0),
      removeSatisfiedTriggers = removeSatisfiedTriggers,
      inputString = inputString,
      accumulatedPayments = accumulatedPayments,
      simulatedFrontier
    )
  
  
}

