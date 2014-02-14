package squantlib.schedule.call

import scala.collection.immutable.ListMap
import squantlib.util.DisplayUtils._
import squantlib.schedule.FixingLeg
import squantlib.util.FixingInformation

case class Callability(
    bermudan:Boolean, 
    triggers:Map[String, Double], 
    bonus:Double,
    inputString:String)(implicit val fixingInfo:FixingInformation) extends FixingLeg {
  
  override val variables = triggers.keySet
  
  def isBermuda:Boolean = bermudan 
  
  def underlyings:List[String] = triggers.keys.toList
  
  def triggerValues(underlyings:List[String]):List[Option[Double]] = underlyings.map(triggers.get)
  
  def isTrigger:Boolean = !triggers.isEmpty
  
  override def isFixed = isTrigger && (variables.isEmpty || !preFixings.isEmpty)
  
  def isPriceable:Boolean = !triggers.values.exists(v => v.isNaN || v.isInfinity)
  
  def isEmpty:Boolean = !bermudan && triggers.isEmpty
  
  def fixedTrigger:Option[Boolean] = if (isFixed) Some(triggers.forall{case (k, v) => v <= getFixings(k)}) else None
  
  def isTriggered(f:Map[String, Double]):Boolean = 
    isTriggered || ((triggers.keySet subsetOf f.keySet) && triggers.forall{case (k, v) => v <= f(k)})
    
  def isTriggered:Boolean = 
    if (isTrigger && isFixed) triggers.forall{case (k, v) => v <= getFixings(k)}
    else false
    
  def redemptionAmount:Double = 1.0 + bonus
  
  override def toString:String = 
    List(
	    if (bermudan) "call " else "",
	    if (isTrigger) (triggers.map{case (k, v) => k + ":" + v.asDouble}.mkString(" ")) else "",
	    if (bonus != 0.0) "bonus " + bonus.asPercent(3) else "",
	    if (isEmpty) "no call" else ""
	    ).mkString("") 
}

object Callability {
  
  val empty = Callability(false, ListMap.empty, 0.0, "")(FixingInformation.empty)
}

