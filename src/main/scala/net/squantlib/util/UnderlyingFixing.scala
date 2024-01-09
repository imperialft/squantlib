package net.squantlib.util

import net.squantlib.util.DisplayUtils._
import net.squantlib.database.DB

case class UnderlyingFixing(
  fixings:Map[String, Option[BigDecimal]]
) {

  def getDecimal:Map[String, Option[BigDecimal]] = fixings

  lazy val getDecimalValue:Map[String, BigDecimal] = fixings.collect{case (ul, Some(v)) => (ul, v)}

  lazy val doubleFixings:Map[String, Double] = fixings.map{case (ul, v) => (ul, v.collect{case v => v.toDouble}.getOrElse(Double.NaN))}

  def nonEmptyFixing:UnderlyingFixing = {
    if (isEmpty) this
    else UnderlyingFixing(fixings.filter{case (ul, v) => v.isDefined})
  }

  def getDouble:Map[String, Double] = doubleFixings

  def isEmpty = fixings.isEmpty

  def size = fixings.size

  def keySet = fixings.keySet

  def keys = fixings.keys

  val isAllValid:Boolean = fixings.values.forall(_.isDefined)

  lazy val isPositive:Boolean = !fixings.isEmpty && fixings.values.forall(v => v.collect{case vv => vv >= 0.0000000000001}.getOrElse(false))

  def isValidFor(underlyingIds:Set[String]):Boolean = underlyingIds.forall(ul => getDecimalValue.contains(ul))

  def getSubset(underlyingIds:Set[String]):UnderlyingFixing = UnderlyingFixing(fixings.filter{case (ul, v) => underlyingIds.contains(ul)})

  def forexNormalized:Map[String, Option[BigDecimal]] = fixings.map{
    case (ul, v) if ul.size == 6 && ul.take(3) == "JPY" && DB.getCurrencyIds.contains(ul.takeRight(3)) =>
      val invertedUl = ul.takeRight(3) + ul.take(3)
      (invertedUl, v.collect{case vv => (1.0 / vv).setScale(DB.getUnderlyingDefaultPrecision(invertedUl), BigDecimal.RoundingMode.HALF_UP)})
    case (ul, v) => (ul, v)
  }

  def toJsonString:String = JsonUtils.jsonString(this)

  def update(newFixings:Map[String, Option[BigDecimal]]):UnderlyingFixing = {
    if (newFixings.isEmpty) this
    else UnderlyingFixing(fixings ++ newFixings)
  }

  def update(newFixings:UnderlyingFixing):UnderlyingFixing = {
    if (newFixings.isEmpty) this
    else UnderlyingFixing(fixings ++ newFixings.fixings)
  }

  override def toString = {
    if (fixings.isEmpty) "(empty)"
    else forexNormalized.map{case (ul, v) => ul + ":" + v.getOrElse("undef")}.mkString(",")
  }

}

object UnderlyingFixing {

  def apply(
    doubleFixings:Map[String, Double]
  )(implicit fixingInfo:FixingInformation):UnderlyingFixing = {
    UnderlyingFixing(doubleFixings.map{case (ul, v) => (ul, v.getDecimal(ul))})
  }

  def apply(
    decimalFixings:Map[String, BigDecimal]
  )(implicit dummyImplicit:DummyImplicit):UnderlyingFixing = UnderlyingFixing(decimalFixings.map{case (ul, v) => (ul, Some(v))})

  def empty:UnderlyingFixing = UnderlyingFixing(Map.empty[String, Option[BigDecimal]])

  def errorValue(uls:Set[String]):UnderlyingFixing = UnderlyingFixing(uls.map(ul => (ul, None)).toMap)

  def flat(uls:Set[String], v: Option[BigDecimal]) = UnderlyingFixing(uls.map(ul => (ul, v)).toMap)
}