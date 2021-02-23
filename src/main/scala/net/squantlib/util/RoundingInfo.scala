package net.squantlib.util

import com.fasterxml.jackson.databind.JsonNode
import net.squantlib.util.JsonUtils._

case class RoundingInfo(precision:Int, roundType:String) {
  def round(v:BigDecimal):BigDecimal = DisplayUtils.ExtendedDecimal.scaled(v, precision, roundType)

  def roundOption(v:Double):Option[BigDecimal] = DisplayUtils.ExtendedDouble.getDecimal(v, precision, roundType)
}

object RoundingInfo {
  def apply(roundPrecision:JsonNode, precisionAdjust:Int = 0):RoundingInfo   = RoundingInfo(
    precision = roundPrecision.parseInt("precision").collect{case r => r + precisionAdjust}.getOrElse(10),
    roundType = roundPrecision.parseString("round_type").getOrElse("rounded")
  )

  def defaultRounding(ccy:String, roundType:String = "rounded"):RoundingInfo = {
    ccy match {
      case "JPY" | "IDR" | "KRW" | "VND" | "CLP" => RoundingInfo (0, roundType)
      case _ => RoundingInfo (2, roundType)
    }
  }
}