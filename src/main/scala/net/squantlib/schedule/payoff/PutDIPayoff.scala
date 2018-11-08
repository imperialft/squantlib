package net.squantlib.schedule.payoff

import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.util.FixingInformation
import net.squantlib.util.Date
import net.squantlib.schedule.CalculationPeriod
import scala.collection.JavaConverters._
import scala.reflect.ClassTag

class PutDIPayoff(
  override val triggers:Map[String, Double],
  override val strikes:Map[String, Double],
  initKnockedIn:Boolean,
  override val physical:Boolean,
  override val reverse: Boolean,
  override val minPayoff:Double,
  override val maxPayoff: Option[Double],
  override val amount:Double = 1.0,
  override val description:String = null,
  override val inputString:String = null
)(implicit override val fixingInfo:FixingInformation) extends PutDIAmericanPayoff(
  triggers = triggers,
  strikes = strikes,
  finalTriggers = triggers,
  refstart = null,
  refend = null,
  knockedIn = initKnockedIn,
  physical = physical,
  forward = false,
  closeOnly = true,
  reverse = reverse,
  minPayoff = minPayoff,
  maxPayoff = maxPayoff,
  amount = amount,
  description = description,
  inputString = inputString
  )
{


  override val variables = triggers.keySet ++ strikes.keySet

  override val strikeOrFinalTriggers:Map[String, Double] = triggers

  override val isPriceable:Boolean =
    !triggers.isEmpty &&
    !triggers.values.exists(v => v.isNaN || v.isInfinity) &&
    !strikes.isEmpty &&
    !strikes.values.exists(v => v.isNaN || v.isInfinity)

  override def eventDates(period:CalculationPeriod):List[Date] = {
    if (!isPriceable) List(period.endDate)
    else if (physical) List(period.eventDate, period.paymentDate)
    else List(period.eventDate)
  }

  override def priceImpl(fixings:List[Map[String, Double]], pastPayments:List[Double], priceResult:PriceResult):Double = priceList(fixings.takeRight(2), priceResult)

  override def toString =
    nominal.asPercent + " [" + triggers.values.map(_.asDouble).mkString(",") + "](Eur) " + nominal.asPercent +
      " x Min([" + variables.mkString(",") + "] / [" + strikes.values.map(_.asDouble).mkString(",") + "])"

  override def jsonMapImpl = Map(
    "type" -> "putdiamerican",
    "variable" -> (triggers.keySet ++ strikes.keySet).toArray,
    "trigger" -> triggers.asJava,
    "strike" -> strikes.asJava,
    "description" -> description)


  override def clearFixings = {
    super.clearFixings
    knockedIn = false
  }

  override def assignFixings(f:Map[String, Double]):Unit = {
    super.assignFixings(f)
    checkKnockIn
  }

  override def checkKnockIn:Unit = {
    knockedIn = implicitly[FixingInterpreter[Map[String, Double]]] isKnockIn(getFixings)
  }

}

object PutDIPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):PutDIPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val fixed = fixingInfo.update(formula)
    val fixedNode = fixed.jsonNode

    val variables:List[String] = formula.parseJsonStringList("variable").map(_.orNull)
    val triggers:Map[String, Double] = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "trigger", variables)}.getOrElse(Map.empty)
    val strikes:Map[String, Double] = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "strike", variables)}.getOrElse(Map.empty)

    val amount:Double = fixed.parseJsonDouble("amount").getOrElse(1.0)
    val description:String = formula.parseJsonString("description").orNull
    val physical:Boolean = formula.parseJsonString("physical").getOrElse("0") == "1"
    val reverse:Boolean = formula.parseJsonString("reverse").getOrElse("0") == "1"
    val minPayoff:Double = fixed.parseJsonDouble("min").getOrElse(0.0)
    val maxPayoff:Option[Double] = fixed.parseJsonDouble("max")
    val knockedIn:Boolean = false

    new PutDIPayoff(
      triggers = triggers,
      strikes = strikes,
      initKnockedIn = knockedIn,
      physical = physical,
      reverse = reverse,
      minPayoff = minPayoff,
      maxPayoff = maxPayoff,
      amount = amount,
      description = description,
      inputString = inputString
    )
  }
  
}

