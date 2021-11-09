package net.squantlib.schedule.baskettypes

import net.squantlib.util.UnderlyingFixing

object BestOf extends BasketType {

  override def isKnockedInPrice(
    fixings:UnderlyingFixing,
    triggers:UnderlyingFixing,
    knockInCheck:(BigDecimal, BigDecimal) => Boolean
  ):Boolean = {
    triggers.keySet.forall(ul => (fixings.getDecimalValue.get(ul), triggers.getDecimalValue.get(ul)) match {
      case (Some(v), Some(stk)) => knockInCheck(v, stk)
      case _ => false
    })
  }

  override def toString:String = "best"

}

