package net.squantlib.schedule.payoff

import scala.collection.JavaConverters._
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.ObjectMapper
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
  inputString:String = null
)(implicit val fixingInfo:FixingInformation) extends Payoff {
  
  override val variables:Set[String] = Set(variable)
  
  override val isPriceable = !payoff.isEmpty

  override def priceImpl(fixings:List[Map[String, Double]], pastPayments:List[Double], priceResult:PriceResult):Double = {
    fixings.lastOption.collect { case f => priceImpl(f, pastPayments, priceResult: PriceResult) }.getOrElse(Double.NaN)
  }

  def priceImpl(fixings:Map[String, Double], pastPayments:List[Double], priceResult:PriceResult):Double = {
    fixings.get(variable) match {
  	  case Some(f) if !f.isNaN && !f.isInfinity => payoff.map(_.price(f)).sum
      case _ => Double.NaN
    }
  }

  override def toString = payoff.map(p => p.toString(variable)).mkString(" ")
  
  override def priceImpl(priceResult:PriceResult) = Double.NaN
  
  override def jsonMapImpl = {
    val jsonPayoff:Array[JavaMap[String, Any]] = payoff.toArray.map(p => {
      val leg:JavaMap[String, Any] = Map(
        "minrange" -> p.minRange.getOrElse("None"),
        "maxrange" -> p.maxRange.getOrElse("None"),
        "mult" -> p.coeff.getOrElse("None"),
        "add" -> p.constant.getOrElse("None")
        ).asJava
      leg
    })
      
    Map(
      "type" -> "leps1d",
      "variable" -> variable,
      "description" -> description,
      "payoff" -> jsonPayoff
    )
    
  }  
  
}


object LEPS1dPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):LEPS1dPayoff = {
    
    if (inputString.startsWith("leps")) {
      val formulalist = FormulaParser.parseList(fixingInfo.update(inputString).substring(4))
      val variables = formulalist.map{case (f, _, _) => f.keySet}.flatten.flatten.toSet
      assert(variables.size <= 1)
      
      val variable = Set(variables.head)
      val underlyingId = variables.head

      val components = formulalist.map{case (formula, minrange, maxrange) => {
        val coeff = formula.get(variable)
        val const = formula.get(Set.empty)

        LEPS1dComponent(
          underlyingId,
          coeff,
          const,
          minrange.flatMap{case v => v.getDecimal(underlyingId)},
          maxrange.flatMap{case v => v.getDecimal(underlyingId)}
        )
      }}

      LEPS1dPayoff(underlyingId, components, inputString)
    }
    
    else {
      val formula = Payoff.updateReplacements(inputString)
      val variable:String = inputString.parseJsonString("variable").orNull
      val description:String = inputString.parseJsonString("description").orNull
    
      val payoff:List[LEPS1dComponent] = fixingInfo.update(formula).jsonNode match {
        case Some(node) => getLEPScomponents(variable, node.get("payoff"))
        case None => List.empty
      }
      
      LEPS1dPayoff(
        variable,
        payoff,
        description,
        inputString
      )
    }
  }

  def apply(
    variable:String,
    payoff:JsonNode,
    description:String
  )(implicit fixingInfo:FixingInformation):LEPS1dPayoff = {
    LEPS1dPayoff(
      variable,
      getLEPScomponents(variable, payoff),
      description,
      payoff.toJsonString
    )
  }
  
  def getLEPScomponents(
    underlyingId:String,
    node:JsonNode
  )(implicit fixingInfo:FixingInformation):List[LEPS1dComponent] = node.parseList.map(vs => LEPS1dComponent(underlyingId, vs))
    
}


case class LEPS1dComponent (
  underlyingId:String,
  coeff:Option[Double],
  constant:Option[Double],
  minRange:Option[BigDecimal],
  maxRange:Option[BigDecimal]
)(implicit val fixingInfo:FixingInformation) {
   
  def price(fixing:Double):Double = {
    minRange match {
      case Some(f) if fixing ~< (f, underlyingId) => return 0.0
      case _ =>
    }
    
    maxRange match {
      case Some(c) if fixing ~>= (c, underlyingId) => return 0.0
      case _ =>
    }
     
    (coeff, constant) match {
      case (None, None) => 0.0
      case (None, Some(c)) => c
      case (Some(x), None) => x * fixing
      case (Some(x), Some(c)) => x * fixing + c
    }
  }
  
  def toString(variable:String) = {
    "[" + minRange.collect { case v => v.asDouble }.getOrElse("") + ", " + maxRange.collect{case v => v.asDouble}.getOrElse("") + "] " + linearFormula(coeff, variable, constant)
  }
  
}

object LEPS1dComponent {
  
  def apply(underlyingId:String, subnode:JsonNode)(implicit fixingInfo:FixingInformation):LEPS1dComponent = {
    val coeff:Option[Double] = Some(subnode.parseDouble("mult").getOrElse(0.0))
    val constant:Option[Double] = subnode.parseDouble("add")
    val minRange:Option[BigDecimal] = subnode.parseDecimal("minrange")
    val maxRange:Option[BigDecimal] = subnode.parseDecimal("maxrange")

    LEPS1dComponent(
      underlyingId,
      coeff,
      constant,
      minRange.collect{case v => v.scaled(underlyingId)},
      maxRange.collect{case v => v.scaled(underlyingId)}
    )
  }
}
