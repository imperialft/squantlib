package net.squantlib.schedule.call

import net.squantlib.util.DisplayUtils._
import net.squantlib.schedule.{CalculationPeriod, FixingLeg, KnockInCondition}
import net.squantlib.util.{Date, FixingInformation, JsonUtils, UnderlyingFixing}
import net.squantlib.schedule.payoff._

case class Callability(
  bermudanCondition: BermudanCondition,
  triggerCondition: TriggerCondition,
  targetRedemptionCondition: TargetRedemptionCondition,
  forward: UnderlyingFixing,
  bonusAmount: BigDecimal,
  inputString: Map[String, Any],
  var accumulatedPayments: Option[Double],
  var simulatedFrontier: UnderlyingFixing = UnderlyingFixing.empty
)(implicit val fixingInfo: FixingInformation) extends FixingLeg {

  def triggers:UnderlyingFixing = triggerCondition.getStrikes

  val triggerVariables:Set[String] = triggers.keySet

  val forwardVariables:Set[String] = forward.keySet

  var issuerCalled:Option[Boolean] = None

  def setIssuerCalled(setTrue:Boolean = true) = {
    issuerCalled = Some(setTrue)
  }

  def optionalTriggers:Option[UnderlyingFixing] = {
    if (isFixed) {
      if (isTriggered(getFixings)) Some(UnderlyingFixing.flat(triggers.keySet, Some(BigDecimal(0.0))))
      else None
    }
    else if (isTrigger) Some(triggers)
    else None
  }

  val optionalForwardStrikes:Option[UnderlyingFixing] = {
    if (forward.isEmpty) None
    else Some(forward)
  }

  override val variables:Set[String] = triggerVariables ++ forwardVariables
  
  def triggerInputString:Map[String, String] = inputString.get("trigger") match {
    case Some(trig:Map[_, _]) => trig.map{
      case (k:String, v:String) => (k, v)
      case (k, v) => (k.toString, v.toString)
    }
    case _ => Map.empty
  }
  

  def underlyings:Set[String] = variables
  
  def triggerValues(underlyings:List[String]):List[Option[BigDecimal]] = underlyings.map(ul => triggers.getDecimalValue.get(ul))

  def isBermuda:Boolean = bermudanCondition.isActive

  def isTrigger:Boolean = triggerCondition.isActive
  
  def isForward:Boolean = !forward.isEmpty
  
  def isTargetRedemption:Boolean = targetRedemptionCondition.isActive
  
  override def isFixed = isFixedTrigger || isFixedTargetRedemption || isEmpty
  
  def isFixedTrigger:Boolean = isTrigger && (variables.isEmpty || (!preFixings.isEmpty && !isFutureFixing))
  
  def isFixedTargetRedemption:Boolean = isTargetRedemption && accumulatedPayments.isDefined && !isFutureFixing
  
  def isPriceable:Boolean = (forward.isEmpty || forward.isPositive) && triggers.isAllValid

  def isEmpty:Boolean = !bermudanCondition.isActive && !triggerCondition.isActive && !targetRedemptionCondition.isActive
  
  def fixedTriggerByTrigger:Option[Boolean] = {
    if (isFixed && isTrigger)
      Some(triggerCondition.isTriggered(getFixings))
    else
      None
  }

  def fixedTriggerByTargetRedemption:Option[Boolean] = {
    if (targetRedemptionCondition.isActive) accumulatedPayments match {
      case Some(acc) => Some(targetRedemptionCondition.isTriggered(acc))
      case _ => None
    } else None
  }

  def fixedTrigger:Option[Boolean] = {
    if (issuerCalled == Some(true)) Some(true)
    else fixedTriggerByTrigger match {
      case Some(t) => Some(t)
      case None => fixedTriggerByTargetRedemption
    }
  }

  def eventDate(d:CalculationPeriod):List[Date] = {
    List(d.callEventDate)
  }

  def isTriggered(f:Map[String, Double]):Boolean = isTriggered(UnderlyingFixing(f))

  def isTriggered(f:UnderlyingFixing):Boolean = triggerCondition.isTriggered(if (isFixed) getFixings else f)

  def isTriggered:Boolean = {
    if (isFixed) isTriggered(getFixings)
    else false
  }

  def isProbablyCalled(f:UnderlyingFixing):Boolean = {
    isTriggered(f) ||
    fixedTriggerByTargetRedemption.getOrElse(false) ||
    (
      bermudanCondition.isActive &&
      !simulatedFrontier.isEmpty &&
      f.isValidFor(simulatedFrontier.keySet) &&
      triggerCondition.isTriggered(simulatedFrontier)
    )
  }
    
  def isProbablyCalled:Boolean = {
    if (isFixed) isProbablyCalled(getFixings)
    else false
  }
     
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
    
  def fixedRedemptionAmount:Option[Double] = {
    if (isFixed && isProbablyCalled) Some(redemptionAmount(getFixings))
    else None
  }
  
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
      bermudanCondition = bermudanCondition,
      triggerCondition = triggerCondition.strikeShifted(shiftedTriggers),
      targetRedemptionCondition = targetRedemptionCondition,
      forward = forward,
      bonusAmount = bonusAmount,
      inputString = inputString,
      accumulatedPayments = accumulatedPayments,
      simulatedFrontier = simulatedFrontier
    )
  }


  override def toString:String =
    List(
	    if (isBermuda) "call " else "",
	    if (isTrigger) triggers else "",
	    if (isTrigger) {if (triggerCondition.triggerUp) "up" else "down"} else "",
	    targetRedemptionCondition.target.collect{case t => "target : " + t.asPercent}.getOrElse(""),
	    if (bonusAmount != 0.0) "bonus " + bonusAmount.asPercent(3) else "",
	    if (forward.isEmpty) "" else "forward " + forward,
	    if (isEmpty) "no call" else "",
      if (triggerCondition.removeSatisfiedTriggers) "memory" else ""
	    ).mkString(" ") 


}

object Callability {
  
  val empty = Callability(
    bermudanCondition = BermudanCondition.empty,
    triggerCondition = TriggerCondition.empty,
    targetRedemptionCondition = TargetRedemptionCondition.empty,
    forward = UnderlyingFixing.empty,
    bonusAmount = 0.0,
    inputString = Map.empty[String, Any],
    accumulatedPayments = None,
    simulatedFrontier= UnderlyingFixing.empty
  )(FixingInformation.empty("JPY", "JPY"))

  def apply(
    bermudan:Boolean,
    triggers: UnderlyingFixing,
    targetRedemption:Option[BigDecimal],
    callOption: CallOption,
    resetCondition: KnockInCondition,
    resetStrikes: UnderlyingFixing,
    inputString:Map[String, Any],
    accumulatedPayments:Option[Double],
    simulatedFrontier:UnderlyingFixing
  )(implicit fixingInfo:FixingInformation):Callability = 
    Callability(
      bermudanCondition = BermudanCondition(bermudan),
      triggerCondition = TriggerCondition.initialize(
        strikes = triggers,
        triggerUp = callOption.triggerUp,
        removeSatisfiedTriggers = callOption.removeSatisfiedTriggers,
        resetCondition = resetCondition,
        resetStrikes = resetStrikes
      ),
      targetRedemptionCondition = TargetRedemptionCondition(targetRedemption),
      forward = callOption.forward,
      bonusAmount = callOption.bonus,
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
    resetCondition: KnockInCondition,
    resetStrikes: UnderlyingFixing,
    inputString:Map[String, Any],
    accumulatedPayments:Option[Double],
    simulatedFrontier:UnderlyingFixing
  )(implicit fixingInfo:FixingInformation):Callability =
    Callability(
      bermudanCondition = BermudanCondition(bermudan),
      triggerCondition = TriggerCondition.initialize(
        strikes = UnderlyingFixing(underlyings.zip(triggers).collect{case (k, Some(v)) => (k, v)}.toMap),
        triggerUp = triggerUp,
        removeSatisfiedTriggers = removeSatisfiedTriggers,
        resetCondition = resetCondition,
        resetStrikes = resetStrikes
      ),
      targetRedemptionCondition = TargetRedemptionCondition(targetRedemption.flatMap{case v => v.getRoundedDecimal}),
      forward = forward,
      bonusAmount = bonusAmount.getRoundedDecimal.getOrElse(0.0),
      inputString = inputString,
      accumulatedPayments = accumulatedPayments,
      simulatedFrontier
    )
  
  
}
