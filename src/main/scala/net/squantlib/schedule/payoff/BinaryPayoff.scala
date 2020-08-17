package net.squantlib.schedule.payoff

import net.squantlib.util.UnderlyingFixing

import scala.language.postfixOps
//import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import com.fasterxml.jackson.databind.ObjectMapper
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import java.util.{Map => JavaMap}
import net.squantlib.util.FixingInformation

/**
 * Interprets JSON formuimport net.squantlib.schedule.payoff.Payoff
la specification for sum of linear formulas with discrete range.
 * JSON format:
 *  {type:"binary", variable:[string], payoff:[{amount:double, strike:[double]}], description:String}, 
 * No strike is considered as no low boundary
 */
case class BinaryPayoff(
  payoff:Set[(BigDecimal, UnderlyingFixing, UnderlyingFixing)],
  memory:Boolean,
  memoryAmount:BigDecimal,
  description:String = null,
  inputString:String = null
)(implicit val fixingInfo:FixingInformation) extends Payoff {

  override val variables: Set[String] = payoff.map { case (k, minK, maxK) => minK.keySet ++ maxK.keySet}.flatten

  val unconditionalAmount:BigDecimal = payoff.filter{
    case (r, minK, maxK) => minK.isEmpty && maxK.isEmpty
  }.map{
    case (r, minK, maxK) => r
  }.reduceOption(_ min _).getOrElse(0.0)

  override val isPriceable: Boolean = !payoff.isEmpty && payoff.forall{case (amount, stkLow, stkHigh) =>
    stkLow.isAllValid && stkHigh.isAllValid
  }

  private val smallValue = 0.0000001

  override def priceImpl(
    fixings:List[UnderlyingFixing],
    pastPayments:List[Double],
    priceResult:PriceResult
  ):Double = fixings.lastOption.collect{case f => priceImpl(f, pastPayments, priceResult)}.getOrElse(Double.NaN)

  def priceImpl(fixings: UnderlyingFixing, pastPayments:List[Double], priceResult:PriceResult):Double = {
    if (isPriceable && fixings.isValidFor(variables)) {
      val cpnRate:Double = payoff.map { case (r, minK, maxK) =>
        if (minK.getDecimal.exists { case (k, v) => fixings.getDecimal(k) < v } || maxK.getDecimal.exists{case (k, v) => fixings.getDecimal(k) > v}) 0.0
        else r.toDouble
      }.max

      if (memory && cpnRate > unconditionalAmount + smallValue) {
        cpnRate + pastPayments.takeWhile(pp => pp < memoryAmount - smallValue).size.toDouble * memoryAmount.toDouble
      } else cpnRate

    } else Double.NaN
  }

  override def toString =
    if (payoff.isEmpty) description
    else payoff.map{
      case (v, minK, maxK) if minK.isEmpty && maxK.isEmpty => v.asPercent
      case (v, minK, maxK) if maxK.isEmpty => " [" + minK + "]" + v.asPercent
      case (v, minK, maxK) if minK.isEmpty => v.asPercent + " [" + maxK + "]"
      case (v, minK, maxK) => " [" + minK + "]" + v.asPercent + " [" + maxK + "]"
    }.mkString(" ")
  
  override def priceImpl(priceResult:PriceResult) = Double.NaN

  def jsonPayoffOutput:Set[Map[String, Any]] = payoff.map{
    case (v, minK, maxK) if minK.isEmpty && maxK.isEmpty =>
      Map(
        "amount" -> v
      )

    case (v, minK, maxK) if maxK.isEmpty =>
      Map(
        "amount" -> v,
        "strike" -> minK.getDouble.map{case (ul, v) => (ul, v)}
      )

    case (v, minK, maxK) if minK.isEmpty =>
      Map(
        "amount" -> v,
        "strike_high" -> maxK.getDouble.map{case (ul, v) => (ul, v)}
      )

    case (v, minK, maxK) =>
      Map(
        "amount" -> v,
        "strike" -> minK.getDouble.map{case (ul, v) => (ul, v)},
        "strike_high" -> maxK.getDouble.map{case (ul, v) => (ul, v)}
      )
  }

  override def jsonMapImpl:Map[String, Any] = {
    Map(
      "type" -> "binary",
      "variable" -> payoff.map { case (_, minK, maxK) => minK.keySet ++ maxK.keySet}.flatten,
      "description" -> description,
      "payoff" -> jsonPayoffOutput.toArray.map(_.asJava),
      "memory" -> (if (memory) "1" else "0"),
      "memory_amount" -> memoryAmount
    )
  }

  override def fixedConditions:Map[String, Any] = {
    Map(
      "payoff" -> jsonPayoffOutput.toArray.map(_.asJava)
    )
  }
        
}

object BinaryPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):BinaryPayoff = {
    val formula = Payoff.updateReplacements(inputString)

    val variable:List[String] = formula.parseJsonStringList("variable").map(_.orNull)

    val reverse:Boolean = formula.parseJsonString("reverse").getOrElse("0") == "1"

    val payoffs:Set[(BigDecimal, UnderlyingFixing, UnderlyingFixing)] = fixingInfo.update(formula).jsonNode("payoff") match {
      case None => Set.empty
  	  case Some(subnode) if subnode.isArray =>
        subnode.asScala.map(n => {
          val amount = n.parseDecimal("amount").getOrElse(BigDecimal(0.0))
          val strikes:UnderlyingFixing = UnderlyingFixing(Payoff.nodeToComputedMap(n, (if (reverse) "strike_low" else "strike"), variable))(fixingInfo.getStrikeFixingInformation)
          val strikeHighs:UnderlyingFixing = UnderlyingFixing(Payoff.nodeToComputedMap(n, (if (reverse) "strike" else "strike_high"), variable))(fixingInfo.getStrikeFixingInformation)
          (amount, strikes, strikeHighs)
        }) (collection.breakOut)
	    case _ => Set.empty
    }

    val memory:Boolean = formula.parseJsonString("memory").getOrElse("0") == "1"

    val memoryAmount:BigDecimal = formula.parseJsonDecimal("memory_amount").getOrElse(BigDecimal(0.0))

    val description:String = formula.parseJsonString("description").orNull

	  BinaryPayoff(
      payoff = payoffs,
      memory = memory,
      memoryAmount = memoryAmount,
      description = description,
      inputString = inputString
    )

  }
  
}