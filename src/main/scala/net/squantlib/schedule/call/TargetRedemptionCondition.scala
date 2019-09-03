package net.squantlib.schedule.call

case class TargetRedemptionCondition(
  target: Option[BigDecimal]
){

  val isActive:Boolean = target match {
    case Some(v) => v > 0.0
    case _ => false
  }

  def isTriggered(accumulatedPayments:Double):Boolean = target match {
    case Some(tgt) => accumulatedPayments >= tgt - 0.0000000001
    case _ => false
  }

}

object TargetRedemptionCondition{

  def empty = TargetRedemptionCondition(None)
}




