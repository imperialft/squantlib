package net.squantlib.schedule.payoff

import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.util.{FixingInformation, FormulaParser, UnderlyingFixing}

import scala.collection.JavaConverters._

/**
 * Interprets general formula as the sum of weighted variables + constant, with cap and floor.
 * Any symbols other than +, -, *, /, > and < are considered as an independent variable.
 * Brackets are not supported.
 */
case class GeneralPayoff(
  formula:Map[Set[String], Double],
  override val minPayoff:Double,
  override val maxPayoff:Option[Double],
  description:String = null,
  inputString:String = null
)(implicit val fixingInfo:FixingInformation) extends Payoff {
  
  override val variables:Set[String] = formula.keySet.flatten
  
  override val isPriceable = true //formula.values.forall(v => !v.isNaN && !v.isInfinity)
  
  override def isFixed = variables.size == 0 || super.isFixed
  
  def leverage(index:String):Double = leverage(Set(index))
  def leverage(index:Set[String]):Double = formula.get(index).collect{case v => v.toDouble}.getOrElse(0.0)
  
  val constant:Double = formula.get(Set.empty).collect{case v => v.toDouble}.getOrElse(0.0)
  
//  override def priceImpl(fixing:Double, pastPayments:List[Double]) =
//    if (variables.size == 1 && !fixing.isNaN && !fixing.isInfinity) price(Map(variables.head -> fixing), pastPayments)
//    else Double.NaN

  override def priceImpl(fixings:List[UnderlyingFixing], pastPayments:List[Double], priceResult:PriceResult):Double = {
    fixings.lastOption.collect { case f => priceImpl(f, pastPayments, priceResult: PriceResult) }.getOrElse(Double.NaN)
  }

  def priceImpl(fixings:UnderlyingFixing, pastPayments:List[Double], priceResult:PriceResult):Double = {
    if (fixings.isValidFor(variables)) {
      var rate = formula.map {
        case (vs, c) if vs.isEmpty => c
        case (vs, c) => vs.toList.map(fixings.getDouble).product * c
      }.sum

      withMinMax(rate)
    } else {
      Double.NaN
    }
  }
   
  override def priceImpl(priceResult:PriceResult):Double = {
    if (variables.isEmpty) constant
    else Double.NaN
  }
  
  override def toString:String = {
    formula.map { case (variables, coeff) =>
      ((if (variables.size > 0) variables.mkString("*") + "*" + coeff.toDouble else coeff.asPercent))
    }.mkString("+").replace("+-", "+")
  }

  override def jsonMapImpl = Map.empty
  
}
 
object GeneralPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):Payoff = {
    val fixedString = fixingInfo.update(inputString)

    val formula = if (inputString.startsWith("{") && inputString.parseJsonString("type") == Some("general")) {
      var payoffString = fixedString.parseJsonString("payoff") match { case None => ""; case Some(n) => n}
      fixedString.jsonNode match {
        case Some(node) => 
          val fieldnames = node.fieldNames.asScala.filter(n => !List("type", "description", "payoff", "variable").contains(n))
          fieldnames.foreach(n => payoffString = payoffString.replace(n, node.get(n).parseDouble.getOrElse(Double.NaN).toString))
        case None => {}
      }
      payoffString
    } else fixedString
    
    val description =  if (fixedString.startsWith("{")) fixedString.parseJsonString("description").orNull else null
    
    val (parsedformula, floor, cap) = FormulaParser.parse(formula)
    
    val variables = parsedformula.keySet.flatten
    val constant = parsedformula.getOrElse(Set.empty, 0.0)
    
    variables.size match {
      case 0 if parsedformula contains Set.empty =>
        FixedPayoff(
          payoff = constant,
          description = description,
          inputString = inputString
        )

      case 0 => NullPayoff(description, inputString)
//      case 1 => {
//        val variable = variables.head
//        Linear1dPayoff(variable, Some(parsedformula.getOrElse(Set(variable), 0.0)), Some(constant), floor, cap, description, inputString)
//      }
      case _ =>
        GeneralPayoff(
          formula = parsedformula,
          minPayoff = floor.getOrElse(0.0),
          maxPayoff = cap,
          description = description,
          inputString = inputString
        )
    }
    
  }

}

