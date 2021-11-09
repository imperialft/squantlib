package net.squantlib.schedule.baskettypes

import net.squantlib.util.UnderlyingFixing

object WorstOf extends BasketType {

  override def isKnockedInPrice(
    fixings:UnderlyingFixing,
    triggers:UnderlyingFixing,
    knockInCheck:(BigDecimal, BigDecimal) => Boolean
  ):Boolean = {
    triggers.keySet.exists(ul => (fixings.getDecimalValue.get(ul), triggers.getDecimalValue.get(ul)) match {
      case (Some(v), Some(stk)) => knockInCheck(v, stk)
      case _ => false
    })
  }

  override def toString:String = "worst"

}

