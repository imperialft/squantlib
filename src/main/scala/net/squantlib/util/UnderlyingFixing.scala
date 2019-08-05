package net.squantlib.util

import net.squantlib.util.DisplayUtils._

case class UnderlyingFixing(
  fixings:Map[String, Option[BigDecimal]]
) {

  def getDecimal:Map[String, Option[BigDecimal]] = fixings

  lazy val doubleFixings:Map[String, Double] = fixings.map{case (ul, v) => (ul, v.collect{case v => v.toDouble}.getOrElse(Double.NaN))}

  def getDouble:Map[String, Double] = doubleFixings

  def isEmpty = fixings.isEmpty

  def size = fixings.size

  def keySet = fixings.keySet

}

object UnderlyingFixing {

  def apply(
    doubleFixings:Map[String, Double]
  )(implicit fixingInfo:FixingInformation):UnderlyingFixing = {
    UnderlyingFixing(doubleFixings.map{case (ul, v) => (ul, v.getDecimal(ul))})
  }

  def apply(
    decimalFixings:Map[String, BigDecimal]
  ):UnderlyingFixing = UnderlyingFixing(decimalFixings.map{case (ul, v) => (ul, Some(v))})

  def empty:UnderlyingFixing = new UnderlyingFixing(Map.empty)

  def errorValue(uls:Set[String]):UnderlyingFixing = new UnderlyingFixing(uls.map(ul => (ul, None)).toMap)


}