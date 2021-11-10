package net.squantlib.schedule.baskettypes

import net.squantlib.util.UnderlyingFixing

trait BasketType {

  def isKnockedInPrice(
    fixings:UnderlyingFixing,
    triggers:UnderlyingFixing,
    knockInCheck:(BigDecimal, BigDecimal) => Boolean
  ):Boolean

  def toString:String

}

object BasketType {

  def parseString(s:String):BasketType = parseString(s, WorstOf)

  def parseString(s:String, defaultType: BasketType):BasketType = {
    s match {
      case "max" | "best" => BestOf
      case "min" | "worst" => WorstOf
      case _ => defaultType
    }
  }

}

