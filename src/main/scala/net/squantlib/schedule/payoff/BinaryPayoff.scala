package net.squantlib.schedule.payoff

import scala.language.postfixOps
//import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import org.codehaus.jackson.map.ObjectMapper
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
  description:String = null,
  inputString:String = null,
  memory:Boolean = false,
  memoryAmount:Double = 0.0
)(implicit val fixingInfo:FixingInformation) extends Payoff {

  override val variables: Set[String] = payoff.map { case (k, minK, maxK) => minK.keySet ++ maxK.keySet}.flatten

  override val isPriceable: Boolean = !payoff.isEmpty &&
    !payoff.exists{ case (k, minK, maxK) =>
      k.isNaN || k.isInfinity || minK.exists{case (kk, vv) => vv.isNaN || vv.isInfinity} || maxK.exists {case (kk, vv) => vv.isNaN || vv.isInfinity}
    }

  //  def getFixings(fixings:Map[String, Double]):Option[List[Double]] =
  //    if (variables.toSet subsetOf fixings.keySet)
  //    Some((0 to binaryVariables.size - 1).map(i => fixings(binaryVariables(i)))(collection.breakOut))
  //    else None

  override def priceImpl(fixings: Map[String, Double], pastPayments:List[Double]) = {
    if (isPriceable && (variables subsetOf fixings.keySet) && fixings.values.forall(v => !v.isNaN && !v.isInfinity)) {
      val cpnRate = payoff.map { case (r, minK, maxK) =>
        if (minK.exists { case (k, v) => fixings(k) < v } || maxK.exists{case (k, v) => fixings(k) > v}) 0.0
        else r
      }.max

      if (memory) {
        cpnRate + pastPayments.takeWhile(pp => pp < memoryAmount - 0.0000001).size.toDouble * memoryAmount
      } else cpnRate

    } else Double.NaN
  }

  override def priceImpl(fixing:Double, pastPayments:List[Double]) = {
    if (variables.size == 1) priceImpl(Map(variables.head -> fixing), pastPayments)
    else Double.NaN
  }

//
//    if (!isPriceable|| variables.size != 1 || fixing.isNaN || fixing.isInfinity) Double.NaN
//    else payoff.map{
//    case (v, None) => v
//    case (v, Some(l)) if fixing > l.head => v
//    case _ => 0.0}.max
  
  override def toString =
    if (payoff.isEmpty) description
    else payoff.map{
      case (v, minK, maxK) if minK.isEmpty && maxK.isEmpty => v.asPercent
      case (v, minK, maxK) if maxK.isEmpty => " [" + minK.values.map(_.asDouble).mkString(",") + "]" + v.asPercent
      case (v, minK, maxK) if minK.isEmpty => v.asPercent + " [" + maxK.values.map(_.asDouble).mkString(",") + "]"
      case (v, minK, maxK) => " [" + minK.values.map(_.asDouble).mkString(",") + "]" + v.asPercent + " [" + maxK.values.map(_.asDouble).mkString(",") + "]"
    }.mkString(" ")
  
  override def priceImpl = Double.NaN
  
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

    val payoffs:Set[(Double, Map[String, Double], Map[String, Double])] = fixingInfo.update(formula).jsonNode("payoff") match {
      case None => Set.empty
  	  case Some(subnode) if subnode isArray => subnode.asScala.map(n => {
        val amount = n.parseDouble("amount").getOrElse(Double.NaN)
        val strikes = Payoff.nodeToComputedMap(n, "strike", variable)
        val strikeHighs = Payoff.nodeToComputedMap(n, "strike_high", variable)
        (amount, strikes, strikeHighs)
//        if (n.get("strike") == null) (amount, None)
//        else (amount, Some(n.get("strike").asScala.map(s => s.parseDouble.getOrElse(Double.NaN)).toList))
      }) (collection.breakOut)
	    case _ => Set.empty
    }

    val memory:Boolean = formula.parseJsonString("memory").getOrElse("0") == "1"

    val memoryAmount:Double = formula.parseJsonDouble("memory_amount").getOrElse(0.0)

    val description:String = formula.parseJsonString("description").orNull

	  BinaryPayoff(payoffs, description, inputString, memory, memoryAmount)

  }
  
}