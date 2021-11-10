package net.squantlib.schedule.payoff

import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.util.FixingInformation
import net.squantlib.util.{Date, UnderlyingFixing}
import net.squantlib.schedule.CalculationPeriod
import scala.collection.JavaConverters._
import scala.reflect.ClassTag
import net.squantlib.schedule.baskettypes._

class PutDIPayoff(
  override val triggers:UnderlyingFixing,
  override val strikes:UnderlyingFixing,
  initKnockedIn:Boolean,
  override val physical:Boolean,
  override val reverse: Boolean,
  override val leverage:Double,
  override val minPayoff:Double,
  override val maxPayoff: Option[Double],
  knockInOnEqual: Boolean,
  override val amount:Double = 1.0,
  override val description:String = null,
  override val inputString:String = null
)(implicit override val fixingInfo:FixingInformation) extends PutDIAmericanPayoff(
  triggers = triggers,
  strikes = strikes,
  finalTriggers = triggers,
  finalTriggerBasketType = WorstOf,
  refstart = null,
  refend = null,
  refDates = Set.empty,
  knockedIn = initKnockedIn,
  physical = physical,
  forward = false,
  closeOnly = true,
  reverse = reverse,
  leverage = leverage,
  minPayoff = minPayoff,
  maxPayoff = maxPayoff,
  knockInOnEqual = knockInOnEqual,
  amount = amount,
  description = description,
  inputString = inputString
  )
{


  override val variables = triggers.keySet ++ strikes.keySet

  override val strikeOrFinalTriggers:UnderlyingFixing = triggers

  override val isPriceable:Boolean = !triggers.isEmpty && triggers.isAllValid && strikes.isPositive

  override def eventDates(period:CalculationPeriod):List[Date] = {
    if (!isPriceable) List(period.endDate)
    else if (physical) List(period.eventDate, period.paymentDate)
    else List(period.eventDate)
  }

  override def priceImpl(
    fixings:List[UnderlyingFixing],
    pastPayments:List[Double],
    priceResult:PriceResult
  ):Double = priceList(fixings.takeRight(2), priceResult)

  override def toString = s"${nominal.asPercent} [${triggers}](Eur) ${nominal.asPercent} x ${leverage.asPercent} x Min([${variables.mkString(",")}] / [${strikes}])"

  override def jsonMapImpl = Map(
    "type" -> "putdiamerican",
    "variable" -> (triggers.keySet ++ strikes.keySet).toArray,
    "trigger" -> triggers.getDouble.map{case (ul, v) => (ul, v)}.asJava,
    "strike" -> strikes.getDouble.map{case (ul, v) => (ul, v)}.asJava,
    "leverage" -> leverage,
    "description" -> description
  )

  override def fixedConditions:Map[String, Any] = {
    Map(
      "trigger" -> triggers.getDouble.map{case (ul, v) => (ul, v)}.asJava,
      "strike" -> strikes.getDouble.map{case (ul, v) => (ul, v)}.asJava,
    )
  }

  override def clearFixings = {
    super.clearFixings
    knockedIn = false
  }

  override def assignFixings(f:UnderlyingFixing):Unit = {
    super.assignFixings(f)
    checkKnockIn
  }

  override def checkKnockIn:Unit = {
    knockedIn = isKnockIn(getFixings) //implicitly[FixingInterpreter[Map[String, Double]]] isKnockIn(getFixings)
  }

  override def dateShifted(shift:Int):Payoff = new PutDIPayoff(
    triggers = triggers,
    strikes = strikes,
    initKnockedIn = initKnockedIn,
    physical = physical,
    reverse = reverse,
    leverage = leverage,
    minPayoff = minPayoff,
    maxPayoff = maxPayoff,
    knockInOnEqual = knockInOnEqual,
    amount = amount,
    description = description,
    inputString = inputString
  )

}

object PutDIPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):PutDIPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val fixed = fixingInfo.update(formula)
    val fixedNode = fixed.jsonNode

    val variables:List[String] = formula.parseJsonStringList("variable").map(_.orNull)
    val triggers:Map[String, Option[BigDecimal]] = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "trigger", variables).getOptionalDecimal()(fixingInfo.getStrikeFixingInformation)}.getOrElse(Map.empty)
    val strikes:Map[String, Option[BigDecimal]] = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "strike", variables).getOptionalDecimal()(fixingInfo.getStrikeFixingInformation)}.getOrElse(Map.empty)

    val amount:Double = fixed.parseJsonDouble("amount").getOrElse(1.0)
    val description:String = formula.parseJsonString("description").orNull
    val physical:Boolean = formula.parseJsonString("physical").getOrElse("0") == "1"
    val reverse:Boolean = formula.parseJsonString("reverse").getOrElse("0") == "1"
    val leverage:Double = fixed.parseJsonDouble("leverage").getOrElse(1.0)
    val minPayoff:Double = fixed.parseJsonDouble("min").getOrElse(0.0)
    val maxPayoff:Option[Double] = fixed.parseJsonDouble("max")
    val knockInOnEqual:Boolean = formula.parseJsonString("ki_on_equal").getOrElse("1") == "1"
    val knockedIn:Boolean = false

    new PutDIPayoff(
      triggers = UnderlyingFixing(triggers),
      strikes = UnderlyingFixing(strikes),
      initKnockedIn = knockedIn,
      physical = physical,
      reverse = reverse,
      leverage = leverage,
      minPayoff = minPayoff,
      maxPayoff = maxPayoff,
      knockInOnEqual = knockInOnEqual,
      amount = amount,
      description = description,
      inputString = inputString
    )
  }
  
}

