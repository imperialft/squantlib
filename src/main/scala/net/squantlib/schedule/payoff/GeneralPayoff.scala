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
//  formula:Map[Set[String], Double],
//  override val minPayoff:Double,
//  override val maxPayoff:Option[Double],
  payoffs: List[GeneralPayoffFormula],
  basket: String,
  description:String = null,
  inputString:String = null
)(implicit val fixingInfo:FixingInformation) extends Payoff {
  
  override val variables:Set[String] = payoffs.map(po => po.variables).flatten.toSet // formula.keySet.flatten
  
  override val isPriceable = true //formula.values.forall(v => !v.isNaN && !v.isInfinity)
  
  override def isFixed = variables.size == 0 || super.isFixed

  override def priceImpl(fixings:List[UnderlyingFixing], pastPayments:List[Double], priceResult:PriceResult):Double = {
    fixings.lastOption.collect {case f =>
      priceImpl(f, pastPayments, priceResult: PriceResult)
    }.getOrElse(Double.NaN)
  }

  def basketToPrice(vs:List[Double]):Double = basket match {
    case "average" => vs.sum / vs.size.toDouble
    case "max" => vs.max
    case _ => vs.min
  }

  def priceImpl(fixings:UnderlyingFixing, pastPayments:List[Double], priceResult:PriceResult):Double = {
    if (fixings.isValidFor(variables)) {
      val rates = payoffs.map(po => po.price(fixings))
      basketToPrice(rates)

    } else {
      Double.NaN
    }
  }
   
  override def priceImpl(priceResult:PriceResult):Double = {
    if (variables.isEmpty) basketToPrice(payoffs.map(po => po.constant))
    else Double.NaN
  }
  
  override def toString:String = {
    payoffs.map{case po =>
      po.formula.map { case (variables, coeff) =>
        ((if (variables.size > 0) variables.mkString("*") + "*" + coeff.toDouble else coeff.asPercent))
      }.mkString("+").replace("+-", "+")
    }.mkString(", ") + (if (payoffs.size <= 1) "" else s"[${basket}]")
  }

  override def jsonMapImpl = Map.empty

}

case class GeneralPayoffFormula(
  formula: Map[Set[String], Double],
  minPayoff: Double,
  maxPayoff: Option[Double]
) {

  def variables:Set[String] = formula.keySet.flatten
  def constant:Double = formula.getOrElse(Set.empty, 0.0)
  def hasConstant:Boolean = formula.contains(Set.empty)

  def leverage(index:String):Double = leverage(Set(index))
  def leverage(index:Set[String]):Double = formula.getOrElse(index, 0.0)

  //  val constant:Double = formula.get(Set.empty).collect{case v => v.toDouble}.getOrElse(0.0)

  def price(fixings:UnderlyingFixing) = {
    val r = formula.map {
      case (vs, c) if vs.isEmpty => c
      case (vs, c) => vs.toList.map(fixings.getDouble).product * c
    }.sum

    maxPayoff match {
      case Some(v) => Math.min(Math.max(r, minPayoff), v)
      case None => Math.max(r, minPayoff)
    }
  }
}

object GeneralPayoff {
  val payoffColumns = List("payoff", "payoff2", "payoff3")

  def apply(inputString:String)(implicit fixingInfo:FixingInformation):Payoff = {
    val updatedString = fixingInfo.update(inputString)
    val fixedString = {
      if (updatedString.startsWith("{")) updatedString
      else s"{${'"'}type${'"'}:${'"'}general${'"'}, ${'"'}payoff${'"'}:${'"'}${updatedString}${'"'}}"
    }

    val description:String = fixedString.parseJsonString("description").orNull

    val basket:String = fixedString.parseJsonString("basket") match {
      case Some("max") => "max"
      case Some("average") => "average"
      case _ => "min"
    }

    val payoffs:List[GeneralPayoffFormula] = payoffColumns.map{case col =>
      fixedString.parseJsonString(col) match {
        case None => None
        case Some(f) =>
          var payoffString = f
          fixedString.jsonNode match {
            case Some(node) =>
              val fieldNames = node.fieldNames.asScala.filter(n => !List("type", "description", "payoff", "variable").contains(n))
              fieldNames.foreach(n => payoffString = payoffString.replace(n, node.get(n).parseDouble.getOrElse(Double.NaN).toString))
            case None => {}
          }
          val parsedFormula = FormulaParser.parse(payoffString)
          Some(GeneralPayoffFormula(parsedFormula._1, parsedFormula._2.getOrElse(0.0), parsedFormula._3))
      }
    }.flatMap{case s => s}

    val variables = payoffs.map(po => po.variables).flatten

    (variables.size, payoffs.headOption) match {
      case (0, Some(po)) if po.hasConstant =>
        FixedPayoff(
          payoff = po.constant,
          description = description,
          inputString = inputString
        )

      case (0, None) => NullPayoff(description, inputString)

      case _ =>
        GeneralPayoff(
          payoffs,
          basket,
          description,
          inputString
        )
    }
    
  }

}

