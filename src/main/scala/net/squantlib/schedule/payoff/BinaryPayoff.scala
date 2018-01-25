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
  payoff:Set[(Double, Map[String, Double])],
  description:String = null,
  inputString:String = null)(implicit val fixingInfo:FixingInformation) extends Payoff {

  override val variables: Set[String] = payoff.map { case (k, vs) => vs.keySet }.flatten

  override val isPriceable: Boolean = !payoff.isEmpty && !payoff.exists { case (k, vs) => k.isNaN || k.isInfinity || vs.exists { case (kk, vv) => vv.isNaN || vv.isInfinity } }

  //  def getFixings(fixings:Map[String, Double]):Option[List[Double]] =
  //    if (variables.toSet subsetOf fixings.keySet)
  //    Some((0 to binaryVariables.size - 1).map(i => fixings(binaryVariables(i)))(collection.breakOut))
  //    else None

  override def priceImpl(fixings: Map[String, Double]) = {
    if (isPriceable && (variables subsetOf fixings.keySet) && fixings.values.forall(v => !v.isNaN && !v.isInfinity)) {
      payoff.map { case (r, kv) =>
        if (kv.exists { case (k, v) => fixings(k) < v }) 0.0
        else r
      }.max
    } else Double.NaN
  }

  override def priceImpl(fixing:Double) = {
    if (variables.size == 1) priceImpl(Map(variables.head -> fixing))
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
      case (v, kr) if kr.isEmpty => v.asPercent
      case (v, kr) => " [" + kr.values.map(_.asDouble).mkString(",") + "]" + v.asPercent
    }.mkString(" ")
  
  override def priceImpl = Double.NaN
  
  override def jsonMapImpl = {
    
    val jsonPayoff:Array[JavaMap[String, AnyRef]] = payoff.map{
      case (v, kr) if kr.isEmpty => Map("amount" -> v.asInstanceOf[AnyRef]).asJava
      case (v, kr) => Map("amount" -> v.asInstanceOf[AnyRef], "strike" -> kr.toArray.asInstanceOf[AnyRef]).asJava
    }.toArray
    
    Map(
      "type" -> "binary",
      "variable" -> payoff.map { case (k, vs) => vs.keySet }.flatten,
      "description" -> description,
      "payoff" -> jsonPayoff)
  }
        
}

object BinaryPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):BinaryPayoff = {
    val formula = Payoff.updateReplacements(inputString)

    val variable:List[String] = formula.parseJsonStringList("variable").map(_.orNull)

    val payoffs:Set[(Double, Map[String, Double])] = fixingInfo.update(formula).jsonNode("payoff") match {
      case None => Set.empty
  	  case Some(subnode) if subnode isArray => subnode.asScala.map(n => {
        val amount = n.parseDouble("amount").getOrElse(Double.NaN)
        val strikes = Payoff.nodeToComputedMap(n, "strike", variable)
        (amount, strikes)
//        if (n.get("strike") == null) (amount, None)
//        else (amount, Some(n.get("strike").asScala.map(s => s.parseDouble.getOrElse(Double.NaN)).toList))
      }) (collection.breakOut)
	    case _ => Set.empty
    }
	  
    val description:String = formula.parseJsonString("description").orNull
	  BinaryPayoff(payoffs, description, inputString)
  }
  
}