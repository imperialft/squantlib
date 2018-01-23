package net.squantlib.schedule.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.map.ObjectMapper
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import java.util.{Map => JavaMap}
import net.squantlib.util.FixingInformation
import net.squantlib.util.Date
import net.squantlib.schedule.CalculationPeriod
import scala.reflect.ClassTag


/**
 * Interprets JSON formula specification for sum of linear formulas with discrete range.
 * JSON format:
 *  {type:"rangeforward", variable:String, triggerlow:Double, triggerhigh:Double, strike:Double, description:String},
 * No strike is considered as no low boundary
 */
case class RangeForwardPayoff(
    refVariables:List[String],
    triggerLow:List[Option[Double]],
    triggerHigh:List[Option[Double]],
    strikes:List[Option[Double]],
    var knockedIn:Boolean,
    override val physical:Boolean,
    forwardInRange:Boolean = true,
    amount:Double = 1.0,
    description:String = null,
    inputString:String = null)(implicit val fixingInfo:FixingInformation) extends Payoff {

  val variables = refVariables.toSet

  nominal = amount

  val strikeMap:Map[String, Double] = (refVariables zip strikes).collect{case (k, Some(v)) => (k, v)} (collection.breakOut)
  val strikeVariables:Set[String] = strikeMap.keySet

  override val isPriceable:Boolean =
    (triggerHigh ++ triggerLow ++ strikes).forall(_ match {
      case Some(v) => !v.isNaN && !v.isInfinity
      case _ => true
    }) &&
    strikes.exists{case s => s.isDefined} &&
    refVariables.size == triggerLow.size &&
    refVariables.size == triggerHigh.size

  override def eventDates(period:CalculationPeriod):List[Date] = {
    if (!isPriceable) List(period.endDate)
    else if (physical) List(period.eventDate, period.paymentDate)
    else List(period.eventDate)
  }

  trait FixingInterpreter[T] {
    def isKnockIn(fixings:T):Boolean // Method to be implemented
    def price(fixings:T, isKnockedIn:Boolean):Double // Method to be implemented

    def price(fixings:T):Double = {
      if (physical) {
        if (isFixed) price(fixings, knockedIn)
        else Double.NaN
      }
      else price(fixings, isKnockIn(fixings))
    }

    def price(fixings:List[T]):Double = {
      fixings.lastOption match {
        case Some(lastFixing) =>
          if (physical) {
            val fixingSize = fixings.length
            if (isFixed) price(lastFixing, knockedIn)
            else if (fixingSize >= 2) price(lastFixing, isKnockIn(fixings(fixings.length - 2)))
            else Double.NaN
          }
          else price(lastFixing, isKnockIn(lastFixing))
        case None => Double.NaN
      }
    }

    def priceSingle(fixing:Double, isKnockedIn:Boolean):Double = {
      if (variables.size != 1) Double.NaN
      else if (!isKnockedIn) 1.0
      else if (fixing.isNaN || fixing.isInfinity || variables.size != 1 || !isPriceable) Double.NaN
      else strikes.head match {
        case Some(v) => fixing / v
        case _ => Double.NaN
      }
    }

    def isKnockInSingle(fixing:Double):Boolean = {
      if (knockedIn || variables.size != 1) true
      else {
        val r = (triggerLow.head, triggerHigh.head) match {
          case (Some(l), Some(h)) => fixing >= l && fixing <= h
          case (None, Some(h)) => fixing <= h
          case (Some(l), None) => fixing >= l
          case (None, None) => true
        }
        if (forwardInRange) r else !r
      }
    }
//
//    def price(fixings:T):Double = {
//      if (physical) {
//        if (isFixed) price(fixings, knockedIn)
//        else Double.NaN
//      }
//      else price(fixings, isKnockIn(fixings))
//    }
//
//    def price(fixings:List[T]):Double = {
//      fixings.lastOption match {
//        case Some(lastFixing) =>
//          if (physical) {
//            val fixingSize = fixings.length
//            if (isFixed) price(lastFixing, knockedIn)
//            else if (fixingSize >= 2) price(lastFixing, isKnockIn(fixings(fixings.length - 2)))
//            else Double.NaN
//          }
//          else price(lastFixing, isKnockIn(lastFixing))
//        case None => Double.NaN
//      }
//    }
  }

  implicit object MapInterpreter extends FixingInterpreter[Map[String, Double]] {

    def getFixings(fixings: Map[String, Double]): Option[List[Double]] = {
      if (variables.toSet subsetOf fixings.keySet)
        Some((0 to refVariables.size - 1).map(i => fixings(refVariables(i)))(collection.breakOut))
      else None
    }

    override def isKnockIn(fixings:Map[String, Double]):Boolean = {
      if (knockedIn) true
      else getFixings(fixings) match {
        case Some(fixValues) if fixValues.forall(v => !v.isNaN && !v.isInfinity) =>
          val r = (fixValues, triggerLow, triggerHigh).zipped.forall{
            case (f, Some(l), Some(h)) => f >= l && f <= h
            case (f, None, Some(h)) => f <= h
            case (f, Some(l), None) => f >= l
            case (f, None, None) => true
          }
          if (forwardInRange) r else !r

        case _ => true
      }
    }

    def price(fixings:Map[String, Double], isKnockedIn:Boolean):Double = {
      if (!isKnockedIn) 1.0
      else if (
        (strikeVariables subsetOf fixings.keySet) &&
        strikeVariables.forall(v => !fixings(v).isNaN && !fixings(v).isInfinity) &&
        isPriceable) strikeVariables.map(v => fixings(v) / strikeMap(v)).min
      else Double.NaN
    }

  }

  implicit object DoubleInterpreter extends FixingInterpreter[Double] {

    override def isKnockIn(fixing:Double):Boolean = isKnockInSingle(fixing)

    override def price(fixing:Double, isKnockedIn:Boolean):Double = priceSingle(fixing, isKnockedIn)
  }

  def priceSingle[A:FixingInterpreter](fixings:A):Double = implicitly[FixingInterpreter[A]] price fixings

  def priceList[A:FixingInterpreter](fixings:List[A]):Double = implicitly[FixingInterpreter[A]] price fixings

  override def priceImpl(fixings:List[Map[String, Double]]):Double = priceList(fixings)

  override def priceImpl(fixings:Map[String, Double]):Double = priceSingle(fixings)

  override def priceImpl[T:ClassTag](fixings:List[Double]):Double = priceList(fixings)

  override def priceImpl(fixing:Double):Double = priceSingle(fixing)

  override def priceImpl = Double.NaN

  override def clearFixings = {
    super.clearFixings
    knockedIn = false
  }

  override def assignFixings(f:Map[String, Double]):Unit = {
    super.assignFixings(f)
    checkKnockIn
  }

  def checkKnockIn:Unit = {
    knockedIn = implicitly[FixingInterpreter[Map[String, Double]]] isKnockIn(getFixings)
  }



//  override def priceImpl(fixings:Map[String, Double]) =
//    fixings.get(variable) match {
//      case Some(v) if !v.isNaN && !v.isInfinity => priceImpl(v)
//      case None => Double.NaN
//    }
//
//  private def satisfyRange(fixing:Double):Boolean = {
//    val r = (triggerLow, triggerHigh) match {
//      case (Some(l), Some(h)) => fixing >= l && fixing <= h
//      case (None, Some(h)) => fixing <= h
//      case (Some(l), None) => fixing >= l
//      case (None, None) => true
//    }
//    if (forwardInRange) r else !r
//  }
//
//  override def priceImpl(fixing:Double):Double =
//    if (satisfyRange(fixing)) fixing / strike
//    else 1.0
//
//  override def clearFixings = {
//    super.clearFixings
//    knockedIn = false
//  }
//

  override def toString =
    nominal.asPercent +
      " [" + triggerLow.map(s => s.getOrElse("NA")).mkString(",") + "] " +
      " [" + triggerHigh.map(s => s.getOrElse("NA")).mkString(",") + "] " +
      nominal.asPercent +
      " x Min([" + variables.mkString(",") + "] / [" + strikes.map(s => s.getOrElse("NA")).mkString(",") + "])"

//  override def priceImpl = Double.NaN

  override def jsonMapImpl = Map(
    "type" -> "rangeforward",
    "variable" -> variables.toArray,
    "triggerlow" -> triggerLow.map(_.orNull).toArray,
    "triggerhigh" -> triggerHigh.map(_.orNull).toArray,
    "strike" -> strikes.map(_.orNull).toArray,
    "description" -> description
  )

}

object RangeForwardPayoff {

  def apply(inputString:String)(implicit fixingInfo:FixingInformation):RangeForwardPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val fixed = fixingInfo.update(formula)

    val variable:List[String] = formula.parseJsonStringList("variable").map(_.orNull)
    
    val triggerHigh:Option[Double] = fixed.parseJsonDouble("triggerhigh")
    val triggerLow:Option[Double] = fixed.parseJsonDouble("triggerlow")
    val strike:Double = fixed.parseJsonDouble("strike").getOrElse(Double.NaN)
    val amount:Double = fixed.parseJsonDouble("amount").getOrElse(1.0)
    val forwardInRange:Boolean = formula.parseJsonString("range_type").getOrElse("in") != "out"
    val physical:Boolean = formula.parseJsonString("physical").getOrElse("0") == "1"
    val description:String = formula.parseJsonString("description").orNull

    RangeForwardPayoff(variables, triggerLow, triggerHigh, strike, false, physical, forwardInRange, amount, description, inputString)

  }

}
