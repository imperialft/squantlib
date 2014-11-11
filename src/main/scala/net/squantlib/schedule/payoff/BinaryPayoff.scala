package net.squantlib.schedule.payoff

import scala.language.postfixOps
import scala.collection.JavaConversions._
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
  binaryVariables:List[String], 
  payoff:List[(Double, Option[List[Double]])], 
  description:String = null,
  inputString:String = null)(implicit val fixingInfo:FixingInformation) extends Payoff {
  
  override val variables = binaryVariables.toSet
  
  val isInvalid:Boolean = payoff.isEmpty || payoff.exists{
    case (v, Some(lst)) => lst.exists(v => v.isNaN || v.isInfinity) || v.isNaN || v.isInfinity
    case (v, None) => v.isNaN || v.isInfinity
  }
  
  override val isPriceable:Boolean = !isInvalid
  
  def getFixings(fixings:Map[String, Double]):Option[List[Double]] = 
    if (variables.toSet subsetOf fixings.keySet) 
    Some((0 to binaryVariables.size - 1).map(i => fixings(binaryVariables(i)))(collection.breakOut))
    else None
    
  override def priceImpl(fixings:Map[String, Double]) = 
    if (payoff.isEmpty || isInvalid) Double.NaN
    else getFixings(fixings) match {
    case Some(fixValues) if fixValues.forall(v => !v.isNaN && !v.isInfinity) => 
      payoff.map{
      case (v, None) => v
      case (v, Some(l)) if fixValues.corresponds(l) {_ >= _} => v
      case _ => 0.0}.max
    case _ => Double.NaN
    }
    
  override def priceImpl(fixing:Double) =
    if (isInvalid || variables.size != 1 || fixing.isNaN || fixing.isInfinity) Double.NaN
    else payoff.map{
    case (v, None) => v 
    case (v, Some(l)) if fixing > l.head => v
    case _ => 0.0}.max
  
  override def toString =
    if (payoff.isEmpty) description
    else payoff.map{
      case (v, None) => v.asPercent
      case (v, Some(s)) => " [" + s.map(_.asDouble).mkString(",") + "]" + v.asPercent
    }.mkString(" ")
  
  override def priceImpl = Double.NaN
  
  override def jsonMapImpl = {
    
    val jsonPayoff:Array[JavaMap[String, Any]] = payoff.map{
    case (v, None) => val jmap:JavaMap[String, Any] = Map("amount" -> v); jmap
    case (v, Some(s)) => val jmap:JavaMap[String, Any] = Map("amount" -> v, "strike" -> s.toArray); jmap
    }.toArray
    
    val varSet:java.util.List[String] = scala.collection.mutable.ListBuffer(binaryVariables: _*)
    
    Map(
      "type" -> "binary",
      "variable" -> varSet, 
      "description" -> description,
      "payoff" -> jsonPayoff)
    
  }  
        
}

object BinaryPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):BinaryPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val variable:List[String] = formula.parseJsonStringList("variable").map(_.orNull)
	  
    val payoff:List[(Double, Option[List[Double]])] = (fixingInfo.update(formula).jsonNode("payoff") match {
      case None => List.empty
	  case Some(subnode) if subnode isArray => subnode.map(n => {
	    val amount = n.parseDouble("amount").getOrElse(Double.NaN)
	    if (n.get("strike") == null) (amount, None)
	    else (amount, Some(n.get("strike").map(s => s.parseDouble.getOrElse(Double.NaN)).toList))
	  }) (collection.breakOut)
	    case _ => List.empty
    })
	  
    val description:String = formula.parseJsonString("description").orNull
	BinaryPayoff(variable, payoff, description, inputString)
  }
  
}