package net.squantlib.schedule

import net.squantlib.util.FixingInformation
import net.squantlib.util.DisplayUtils._

import scala.collection.LinearSeq

trait FixingLeg {
  
  protected var preFixings:Map[String, BigDecimal] = Map.empty

  protected var futureFixing:Boolean = false

  val fixingInfo:FixingInformation

  val variables:Set[String] // to be implemented
	
  def assignFixings(f:Map[String, BigDecimal]):Unit = {
    if ((variables subsetOf f.keySet) || f.isEmpty) preFixings = f
  }

  def assignFixings(f:Map[String, Double])(implicit fixingInfo:FixingInformation):Unit = assignFixings(f.getDecimal)

  def assignFixings(f:Double)(implicit fixingInfo:FixingInformation):Unit = {
    if (variables.size == 1) assignFixings(Map(variables.head -> f.getDecimal(variables.head)))
  }
	
  def clearFixings = preFixings = Map.empty

  def setFutureFixing = futureFixing = true

  def setPastFixing = futureFixing = false

  def getFixings:Map[String, BigDecimal] = preFixings

  def getDoubleFixings:Map[String, Double] = preFixings.getDouble

  def isFutureFixing:Boolean = futureFixing
	
  def isFixed = variables.isEmpty || (!preFixings.isEmpty && !isFutureFixing)

  protected var settlementFixings:Map[String, BigDecimal] = Map.empty // used for physical settlement only
  
  def isSettlementFixed:Boolean = true
  
  def assignSettlementFixings(f:Map[String, BigDecimal]):Unit = {
    if ((variables subsetOf f.keySet) || f.isEmpty) settlementFixings = f
  }

  def assignSettlementFixings(f:Map[String, Double])(implicit fixingInfo:FixingInformation):Unit = assignSettlementFixings(f.getDecimal)

  def assignSettlementFixings(f:Double)(implicit fixingInfo:FixingInformation):Unit = {
    if (variables.size == 1) assignSettlementFixings(Map(variables.head -> f.getDecimal(variables.head)))
  }

  def clearSettlementFixings = settlementFixings = Map.empty
    
  def getSettlementFixings = settlementFixings
  
}


trait FixingLegs[T <: FixingLeg] {
  
  val fixinglegs:LinearSeq[T]
  
  def assignFixings(fixings:List[Map[String, BigDecimal]]):Unit = {
    assert(fixings.size == fixinglegs.size)
    (fixinglegs, fixings).zipped.foreach{case (p, f) => p.assignFixings(f)}
  }
	
  def assignFixings(fixings:List[Option[Double]]) (implicit fixingInfo:FixingInformation):Unit = {
    assert(fixings.size == fixinglegs.size)
    (fixinglegs, fixings).zipped.foreach{
      case (p, Some(f)) => p.assignFixings(f)
      case (p, None) => {}}
    }
	
  def isFixed:Boolean = fixinglegs.forall(_.isFixed)
  
  def assignSettlementFixings(fixings:List[Map[String, BigDecimal]]):Unit = {
    assert(fixings.size == fixinglegs.size)
    (fixinglegs, fixings).zipped.foreach{case (p, f) => p.assignSettlementFixings(f)}
  }
    
  def assignSettlementFixings(fixings:List[Option[Double]]) (implicit fixingInfo:FixingInformation):Unit = {
    assert(fixings.size == fixinglegs.size)
    (fixinglegs, fixings).zipped.foreach{
      case (p, Some(f)) => p.assignSettlementFixings(f)
      case (p, None) => {}}
    }
  
}

