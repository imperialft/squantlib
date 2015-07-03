package net.squantlib.schedule.call

import scala.collection.immutable.ListMap
import net.squantlib.util.DisplayUtils._
import net.squantlib.schedule.FixingLeg
import net.squantlib.util.FixingInformation

case class Callability(
    bermudan:Boolean, 
    triggers:Map[String, Double], 
    targetRedemption:Option[Double],
    bonus:Double,
    inputString:Map[String, String],
    var accumulatedPayments:Option[Double],
    var simulatedFrontier:Map[String, Double] = Map.empty
    )(implicit val fixingInfo:FixingInformation) extends FixingLeg {
  
  override val variables = triggers.keySet
  
  def isBermuda:Boolean = bermudan 
  
  def underlyings:List[String] = triggers.keys.toList
  
  def triggerValues(underlyings:List[String]):List[Option[Double]] = underlyings.map(triggers.get)
  
  def isTrigger:Boolean = !triggers.isEmpty
  
  def isTargetRedemption:Boolean = targetRedemption.isDefined
  
  override def isFixed = isTrigger && (variables.isEmpty || !preFixings.isEmpty)
  
  def isPriceable:Boolean = !triggers.values.exists(v => v.isNaN || v.isInfinity)
  
  def isEmpty:Boolean = !bermudan && triggers.isEmpty && targetRedemption.isEmpty
  
  def fixedTriggerByTrigger:Option[Boolean] = if (isFixed && isTrigger) Some(triggers.forall{case (k, v) => v <= getFixings(k)}) else None
  
  def fixedTriggerByTargetRedemption:Option[Boolean] = (targetRedemption, accumulatedPayments) match {
    case (Some(tgt), Some(acc)) => Some(acc >= tgt - 0.0000000001)
    case _ => None
  }
  
  def fixedTrigger:Option[Boolean] = fixedTriggerByTrigger match {
    case Some(t) => Some(t)
    case None => fixedTriggerByTargetRedemption
  }

  def judgeTrigger(f:Map[String, Double]):Boolean = 
    isTrigger && !triggers.isEmpty && (triggers.keySet subsetOf f.keySet) && triggers.forall{case (k, v) => v <= f(k)}
  
  def isTriggered(f:Map[String, Double]):Boolean = judgeTrigger(if (isFixed) getFixings else f)
  
  def isTriggered:Boolean = if (isFixed) isTriggered(getFixings) else false
    
  def isProbablyCalled(f:Map[String, Double]):Boolean = 
    isTriggered(f) || (bermudan && !simulatedFrontier.isEmpty && (simulatedFrontier.keySet subsetOf f.keySet) && simulatedFrontier.forall{case (k, v) => v <= f(k)})
    
  def isProbablyCalled:Boolean = if (isFixed) isProbablyCalled(getFixings) else false
    
  def redemptionAmount:Double = 1.0 + bonus
  
  override def toString:String = 
    List(
	    if (bermudan) "call " else "",
	    if (isTrigger) (triggers.map{case (k, v) => k + ":" + v.asDouble}.mkString(" ")) else "",
	    targetRedemption.collect{case t => "target : " + t.asPercent}.getOrElse(""),
	    if (bonus != 0.0) "bonus " + bonus.asPercent(3) else "",
	    if (isEmpty) "no call" else ""
	    ).mkString("") 
}

object Callability {
  
  val empty = Callability(false, ListMap.empty, None, 0.0, Map.empty, None)(FixingInformation.empty)
}

