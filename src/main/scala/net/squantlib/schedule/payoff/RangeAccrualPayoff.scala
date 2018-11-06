package net.squantlib.schedule.payoff

import scala.language.postfixOps
import scala.collection.JavaConversions._
import org.codehaus.jackson.map.ObjectMapper
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import java.util.{Map => JavaMap}
import net.squantlib.util.Date
import net.squantlib.util.FixingInformation

/**
 * Interprets JSON formuimport net.squantlib.schedule.payoff.Payoff
la specification for sum of linear formulas with discrete range.
 * JSON format:
 *  {type:"rangeaccrual", variable:[string], base_amount:Double, bonus_amount:Double, rangelow:Double, rangehigh:Double, refstart:Date, refend:Date, description:String},
 * No strike is considered as no low boundary
 */
case class RangeAccrualPayoff(
  rangeAccrualVariable:String,
  baseAmount: Double,
  bonusAmount: Double,
  rangeLow: Option[Double],
  rangeHigh: Option[Double],
  refstart: Date,
  refend: Date,
  description:String = null,
  inputString:String = null)(implicit val fixingInfo:FixingInformation) extends Payoff {

  // To be implemented

  override val variables:Set[String] = Set(rangeAccrualVariable)

  override val isPriceable = false

  override val isFixed = false

  override def priceImpl(fixings:List[Map[String, Double]], pastPayments:List[Double], priceResult:PriceResult) = Double.NaN

//  override def priceImpl(fixing:Double, pastPayments:List[Double]) = Double.NaN

  override def priceImpl(priceResult:PriceResult) = Double.NaN

  override def toString = description

  override def jsonMapImpl = Map.empty


}

object RangeAccrualPayoff {

  def apply(inputString:String)(implicit fixingInfo:FixingInformation):RangeAccrualPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val fixed = fixingInfo.update(formula)

    val variable:String = formula.parseJsonString("variable").getOrElse(null)
    val baseAmount:Double = fixed.parseJsonDouble("base_amount").getOrElse(0.0)
    val bonusAmount:Double = fixed.parseJsonDouble("bonus_amount").getOrElse(0.0)
    val rangeLow:Option[Double] = fixed.parseJsonDouble("rangelow")
    val rangeHigh:Option[Double] = fixed.parseJsonDouble("rangehigh")
    val refstart:Date = formula.parseJsonDate("refstart").orNull
    val refend:Date = formula.parseJsonDate("refend").orNull
    val description:String = formula.parseJsonString("description").orNull

    RangeAccrualPayoff(
      rangeAccrualVariable = variable,
      baseAmount = baseAmount,
      bonusAmount = bonusAmount,
      rangeLow = rangeLow,
      rangeHigh = rangeHigh,
      refstart = refstart,
      refend = refend,
      description = description,
      inputString = inputString
    )
  }

}
