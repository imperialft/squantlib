package net.squantlib.schedule.call

import net.squantlib.util.DisplayUtils._
import net.squantlib.schedule.{CalculationPeriod, FixingLeg, KnockInCondition}
import net.squantlib.util.{Date, FixingInformation, JsonUtils, UnderlyingFixing}
import net.squantlib.schedule.payoff._
import org.jquantlib.time.Calendar

case class Callability(
  bermudanCondition: BermudanCondition,
  triggerCondition: TriggerCondition,
  targetRedemptionCondition: TargetRedemptionCondition,
  forward: UnderlyingFixing,
  bonusAmount: BigDecimal,
  var exercised: Option[Boolean],
  inputString: Map[String, Any],
  var accumulatedPayments: Option[Double],
  var simulatedFrontier: UnderlyingFixing = UnderlyingFixing.empty
)(implicit val fixingInfo: FixingInformation) extends FixingLeg {

  def triggers:UnderlyingFixing = triggerCondition.getStrikes

  val triggerVariables:Set[String] = triggers.keySet

  val forwardVariables:Set[String] = forward.keySet

  def isIssuerCalled:Boolean = (bermudanCondition.isActive && exercised == Some(true))

  def setIssuerCalled(setTrue:Boolean = true) = {
    exercised = Some(setTrue)
  }

  def isBarrierTriggered:Boolean = triggerCondition.barrierCondition.isKnockedIn

  def barrierTriggeredFixing:Option[UnderlyingFixing] = triggerCondition.barrierCondition.getKnockInFixing

  def barrierTriggeredDate:Option[Date] = triggerCondition.barrierCondition.getKnockInDate

  def adjustedEventDate:Option[Date] = barrierTriggeredDate

  def adjustedPaymentDate:Option[Date] = {
    barrierTriggeredDate.flatMap {
      case d => triggerCondition.redemptionAfter.collect{
        case s => d.advance(fixingInfo.paymentCalendar, s)
      }
    }
  }

  def setAdjustedSchedule(cp: CalculationPeriod, p:Payoff):Boolean = {
    adjustedPaymentDate match {
      case Some(d) =>
        cp.setAdjustedPaymentDate(d)
        if (!triggerCondition.fullCouponOnBarrier) {
          cp.setAdjustedEndDate(d)
        }
      case _ =>
        cp.clearAdjustedPaymentDate
        cp.clearAdjustedEndDate
    }

    adjustedEventDate match {
      case Some(d) =>
        cp.setAdjustedEventDate(d)
        barrierTriggeredFixing.collect{case f => p.assignFixings(f)}
        true
      case _ =>
        cp.clearAdjustedEventDate
        false
    }
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
  
  override def isFixed = isFixedTrigger || isFixedTargetRedemption || isEmpty || isIssuerCalled || isBarrierTriggered
  
  def isFixedTrigger:Boolean = isBarrierTriggered || (isTrigger && (variables.isEmpty || (!preFixings.isEmpty && !isFutureFixing)))
  
  def isFixedTargetRedemption:Boolean = isTargetRedemption && accumulatedPayments.isDefined && !isFutureFixing
  
  def isPriceable:Boolean = (forward.isEmpty || forward.isPositive) && triggers.isAllValid

  def isEmpty:Boolean = !bermudanCondition.isActive && !triggerCondition.isActive && !targetRedemptionCondition.isActive
  
  def fixedTriggerByTrigger:Option[Boolean] = {
    if (isBarrierTriggered) {
      Some(true)
    } else if (isFixed && isTrigger) {
      Some(triggerCondition.isTriggered(getFixings))
    } else {
      None
    }
  }

  def fixedTriggerByTargetRedemption:Option[Boolean] = {
    if (targetRedemptionCondition.isActive) accumulatedPayments match {
      case Some(acc) => Some(targetRedemptionCondition.isTriggered(acc))
      case _ => None
    } else None
  }

  def fixedTrigger:Option[Boolean] = {
    fixedTriggerByTrigger match {
      case Some(t) => Some(t)
      case None => fixedTriggerByTargetRedemption
    }
  }

  def fixedCalled:Boolean = {
    fixedTrigger == Some(true) ||
    isIssuerCalled ||
    fixedTriggerByTargetRedemption == Some(true)
  }

  def eventDate(d:CalculationPeriod):List[Date] = {
    List(d.callEventDate)
  }

  def isTriggered(f:Map[String, Double]):Boolean = isTriggered(UnderlyingFixing(f))

  def isTriggered(f:UnderlyingFixing):Boolean = isBarrierTriggered || triggerCondition.isTriggered(if (isFixed) getFixings else f)

  def isTriggered:Boolean = {
    if (isBarrierTriggered) true
    else if (isFixed) isTriggered(getFixings)
    else false
  }

  def isCalled(f:UnderlyingFixing):Boolean = {
    isTriggered(f) ||
    fixedTriggerByTargetRedemption.getOrElse(false) ||
    isIssuerCalled
  }

  def isCalled:Boolean = {
    if (isFixed) isCalled(getFixings)
    else false
  }

  def isCalledByExerciseFrontier(f:UnderlyingFixing):Boolean = {
    bermudanCondition.isActive &&
    !simulatedFrontier.isEmpty &&
    f.isValidFor(simulatedFrontier.keySet) &&
    triggerCondition.isTriggered(simulatedFrontier)
  }

  def isProbablyCalled(f:UnderlyingFixing):Boolean = {
    isCalled(f) || isCalledByExerciseFrontier(f)
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
        "leverage" -> 1.0,
        "strike" -> inputString.getOrElse("forward", forward)
      ))
      new ForwardPayoff(
        strikes = forward,
        physical = true,
        leverage = 1.0,
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
      exercised = exercised,
      inputString = inputString,
      accumulatedPayments = accumulatedPayments,
      simulatedFrontier = simulatedFrontier
    )
  }

  override def assignFixings(f:UnderlyingFixing):Unit = {
    super.assignFixings(barrierTriggeredFixing.getOrElse(f))
//    isBarrierTriggered
  }

  override def toString:String = {
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
}

object Callability {
  
  val empty = Callability(
    bermudanCondition = BermudanCondition.empty,
    triggerCondition = TriggerCondition.empty,
    targetRedemptionCondition = TargetRedemptionCondition.empty,
    forward = UnderlyingFixing.empty,
    bonusAmount = 0.0,
    exercised = None,
    inputString = Map.empty[String, Any],
    accumulatedPayments = None,
    simulatedFrontier= UnderlyingFixing.empty
  )(FixingInformation.empty("JPY", "JPY"))

  def apply(
    bermudan:Boolean,
    triggers: UnderlyingFixing,
    barrierCondition: KnockInCondition,
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
        resetStrikes = resetStrikes,
        barrierCondition = barrierCondition,
        redemptionAfter = callOption.barrierRedemptionAfter,
        fullCouponOnBarrier = callOption.fullCouponOnBarrier
      ),
      targetRedemptionCondition = TargetRedemptionCondition(targetRedemption),
      forward = callOption.forward,
      bonusAmount = callOption.bonus,
      exercised = callOption.exercised,
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
    exercised: Option[Boolean],
    removeSatisfiedTriggers: Boolean,
    resetCondition: KnockInCondition,
    resetStrikes: UnderlyingFixing,
    barrierCondition: KnockInCondition,
    redemptionAfter: Option[Int],
    fullCouponOnBarrier: Boolean,
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
        resetStrikes = resetStrikes,
        barrierCondition = barrierCondition,
        redemptionAfter = redemptionAfter,
        fullCouponOnBarrier = fullCouponOnBarrier
      ),
      targetRedemptionCondition = TargetRedemptionCondition(targetRedemption.flatMap{case v => v.getRoundedDecimal}),
      forward = forward,
      bonusAmount = bonusAmount.getRoundedDecimal.getOrElse(0.0),
      exercised = exercised,
      inputString = inputString,
      accumulatedPayments = accumulatedPayments,
      simulatedFrontier
    )

  
}
