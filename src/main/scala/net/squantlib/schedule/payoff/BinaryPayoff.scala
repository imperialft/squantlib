package net.squantlib.schedule.payoff

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
  payoff:Set[(Double, Map[String, Double], Map[String, Double])],
  memory:Boolean,
  memoryAmount:Double,
  description:String = null,
  inputString:String = null,
)(implicit val fixingInfo:FixingInformation) extends Payoff {

  override val variables: Set[String] = payoff.map { case (k, minK, maxK) => minK.keySet ++ maxK.keySet}.flatten

  val unconditionalAmount:Double = payoff.filter{case (r, minK, maxK) => minK.isEmpty && maxK.isEmpty}.map{case (r, minK, maxK) => r}.reduceOption(_ min _).getOrElse(0.0)

  override val isPriceable: Boolean =
    !payoff.isEmpty &&
    !payoff.exists{ case (k, minK, maxK) =>
      k.isNaN || k.isInfinity || minK.exists{case (kk, vv) => vv.isNaN || vv.isInfinity} || maxK.exists {case (kk, vv) => vv.isNaN || vv.isInfinity}
    }

  //  def getFixings(fixings:Map[String, Double]):Option[List[Double]] =
  //    if (variables.toSet subsetOf fixings.keySet)
  //    Some((0 to binaryVariables.size - 1).map(i => fixings(binaryVariables(i)))(collection.breakOut))
  //    else None

  private val smallValue = 0.0000001

  override def priceImpl(fixings:List[Map[String, Double]], pastPayments:List[Double], priceResult:PriceResult) =
    fixings.lastOption.collect{case f => priceImpl(f, pastPayments, priceResult)}.getOrElse(Double.NaN)

  def priceImpl(fixings: Map[String, Double], pastPayments:List[Double], priceResult:PriceResult) = {
    if (isPriceable && (variables subsetOf fixings.keySet) && fixings.values.forall(v => !v.isNaN && !v.isInfinity)) {
      val cpnRate = payoff.map { case (r, minK, maxK) =>
        if (minK.exists { case (k, v) => fixings(k) < v } || maxK.exists{case (k, v) => fixings(k) > v}) 0.0
        else r
      }.max

      if (memory && cpnRate > unconditionalAmount + smallValue) {
        cpnRate + pastPayments.takeWhile(pp => pp < memoryAmount - smallValue).size.toDouble * memoryAmount
      } else cpnRate

    } else Double.NaN
  }

  override def toString =
    if (payoff.isEmpty) description
    else payoff.map{
      case (v, minK, maxK) if minK.isEmpty && maxK.isEmpty => v.asPercent
      case (v, minK, maxK) if maxK.isEmpty => " [" + minK.values.map(_.asDouble).mkString(",") + "]" + v.asPercent
      case (v, minK, maxK) if minK.isEmpty => v.asPercent + " [" + maxK.values.map(_.asDouble).mkString(",") + "]"
      case (v, minK, maxK) => " [" + minK.values.map(_.asDouble).mkString(",") + "]" + v.asPercent + " [" + maxK.values.map(_.asDouble).mkString(",") + "]"
    }.mkString(" ")
  
  override def priceImpl(priceResult:PriceResult) = Double.NaN
  
  override def jsonMapImpl = {
    
    val jsonPayoff:Array[JavaMap[String, AnyRef]] = payoff.map{
      case (v, minK, maxK) if minK.isEmpty && maxK.isEmpty => Map("amount" -> v.asInstanceOf[AnyRef]).asJava
      case (v, minK, maxK) if maxK.isEmpty => Map("amount" -> v.asInstanceOf[AnyRef], "strike" -> minK.toArray.asInstanceOf[AnyRef]).asJava
      case (v, minK, maxK) if minK.isEmpty => Map("amount" -> v.asInstanceOf[AnyRef], "strike_high" -> maxK.toArray.asInstanceOf[AnyRef]).asJava
      case (v, minK, maxK) => Map("amount" -> v.asInstanceOf[AnyRef], "strike" -> minK.toArray.asInstanceOf[AnyRef], "strike_high" -> maxK.toArray.asInstanceOf[AnyRef]).asJava
    }.toArray

    Map(
      "type" -> "binary",
      "variable" -> payoff.map { case (k, minK, maxK) => minK.keySet ++ maxK.keySet}.flatten,
      "description" -> description,
      "payoff" -> jsonPayoff,
      "memory" -> (if (memory) "1" else "0"),
      "memory_amount" -> memoryAmount
    )
  }
        
}

object BinaryPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):BinaryPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val fixedNode = formula.jsonNode

    val variable:List[String] = formula.parseJsonStringList("variable").map(_.orNull)

    val reverse:Boolean = formula.parseJsonString("reverse").getOrElse("0") == "1"

    val payoffs:Set[(Double, Map[String, Double], Map[String, Double])] = fixingInfo.update(formula).jsonNode("payoff") match {
      case None => Set.empty
  	  case Some(subnode) if subnode.isArray =>
        subnode.asScala.map(n => {
          val amount = n.parseDouble("amount").getOrElse(Double.NaN)
          val strikes = Payoff.nodeToComputedMap(n, (if (reverse) "strike_low" else "strike"), variable)
          val strikeHighs = Payoff.nodeToComputedMap(n, (if (reverse) "strike" else "strike_high"), variable)
          (amount, strikes, strikeHighs)
        }) (collection.breakOut)
	    case _ => Set.empty
    }

    val memory:Boolean = formula.parseJsonString("memory").getOrElse("0") == "1"

    val memoryAmount:Double = formula.parseJsonDouble("memory_amount").getOrElse(0.0)


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