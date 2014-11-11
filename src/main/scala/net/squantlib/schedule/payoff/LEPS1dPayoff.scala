package net.squantlib.schedule.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.util.FormulaParser
import java.util.{Map => JavaMap}
import scala.Array.canBuildFrom
import net.squantlib.util.FixingInformation

/**
 * Interprets JSON formula specification for sum of linear formulas with discrete range.
 * JSON format:
 * - {type:"leps1d", variable:string, description:String, payoff:formula}, where
 *   formula = Array {minrange:double, maxrange:double, mult:double, add:double}
 *   payment for array(i) is 
 *     if minrange(i) <= X < maxrange(i) => mult(i) * variable + add(i)
 *     otherwise zero
 */
case class LEPS1dPayoff(
    variable:String, 
    payoff:List[LEPS1dComponent], 
    description:String = null,
    inputString:String = null)(implicit val fixingInfo:FixingInformation) extends Payoff {
  
  override val variables:Set[String] = Set(variable)
  
  override val isPriceable = !payoff.isEmpty
  
  override def priceImpl(fixings:Map[String, Double]) = fixings.get(variable) match {
    case Some(f) if !f.isNaN && !f.isInfinity => price(f)
    case _ => Double.NaN
  }
  
  override def priceImpl(fixing:Double) = if (fixing.isNaN || fixing.isInfinity) Double.NaN else payoff.map(_.price(fixing)).sum
  
  override def toString = payoff.map(p => p.toString(variable)).mkString(" ")
  
  override def priceImpl = Double.NaN
  
  override def jsonMapImpl = {
    val jsonPayoff:Array[JavaMap[String, Any]] = payoff.toArray.map(p => {
      val leg:JavaMap[String, Any] = Map(
        "minrange" -> p.minRange.getOrElse("None"),
        "maxrange" -> p.maxRange.getOrElse("None"),
        "mult" -> p.coeff.getOrElse("None"),
        "add" -> p.constant.getOrElse("None")
        )
      leg})
      
    Map(
        "type" -> "leps1d", 
        "variable" -> variable, 
        "description" -> description,
        "payoff" -> jsonPayoff)
    
  }  
  
}


object LEPS1dPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):LEPS1dPayoff = {
    
    if (inputString.startsWith("leps")) {
      val formulalist = FormulaParser.parseList(fixingInfo.update(inputString).substring(4))
      val variables = formulalist.map{case (f, _, _) => f.keySet}.flatten.flatten.toSet
      assert(variables.size <= 1)
      
      val variable = Set(variables.head)
      val components = formulalist.map{case (formula, minrange, maxrange) => {
        val coeff = formula.get(variable)
        val const = formula.get(Set.empty)
        LEPS1dComponent(coeff, const, minrange, maxrange)
      }}
      LEPS1dPayoff(variables.head, components, inputString)
    }
    
    else {
      val formula = Payoff.updateReplacements(inputString)
      val variable:String = inputString.parseJsonString("variable").orNull
      val description:String = inputString.parseJsonString("description").orNull
    
      val payoff:List[LEPS1dComponent] = fixingInfo.update(formula).jsonNode match {
        case Some(node) => getLEPScomponents(node.get("payoff"))
        case None => List.empty
      }
      
      LEPS1dPayoff(variable, payoff, description, inputString)
    }
  }

  def apply(variable:String, payoff:JsonNode, description:String)(implicit fixingInfo:FixingInformation):LEPS1dPayoff = 
    LEPS1dPayoff(variable, getLEPScomponents(payoff), description, payoff.toJsonString)
  
  def getLEPScomponents(node:JsonNode):List[LEPS1dComponent] = node.parseList.map(LEPS1dComponent(_))
    
}


case class LEPS1dComponent (coeff:Option[Double], constant:Option[Double], minRange:Option[Double], maxRange:Option[Double]) {
   
  def price(fixing:Double):Double = {
    minRange match {
      case Some(f) if fixing < f => return 0.0
      case _ =>
    }
    
    maxRange match {
      case Some(c) if fixing >= c => return 0.0
      case _ =>
    }
     
    (coeff, constant) match {
      case (None, None) => 0.0
    case (None, Some(c)) => c
    case (Some(x), None) => x * fixing
    case (Some(x), Some(c)) => x * fixing + c
    }
  }
  
  def toString(variable:String) =     
    "[" + minRange.asDoubleOr("") + ", " + maxRange.asDoubleOr("") + "] " + linearFormula(coeff, variable, constant)
  
}

object LEPS1dComponent {
  
  def apply(subnode:JsonNode):LEPS1dComponent = {
    val coeff:Option[Double] = Some(subnode.parseDouble("mult").getOrElse(1.0))
    val constant:Option[Double] = subnode.parseDouble("add")
    val minRange:Option[Double] = subnode.parseDouble("minrange")
    val maxRange:Option[Double] = subnode.parseDouble("maxrange")
    LEPS1dComponent(coeff, constant, minRange, maxRange)
  }
}
