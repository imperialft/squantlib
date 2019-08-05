package net.squantlib.schedule

import net.squantlib.util.FixingInformation
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.UnderlyingFixing
import scala.collection.LinearSeq

trait FixingLeg {
  
  protected var preFixings:UnderlyingFixing = UnderlyingFixing.empty

  protected var futureFixing:Boolean = false

  val fixingInfo:FixingInformation

  val variables:Set[String] // to be implemented
	
  def assignFixings(f:UnderlyingFixing):Unit = {
    if ((variables subsetOf f.keySet) || f.isEmpty) preFixings = f
  }

//  def assignFixings(f:Map[String, Double])(implicit fixingInfo:FixingInformation):Unit = assignFixings(f.getDecimal)
//
//  def assignFixings(f:Double)(implicit fixingInfo:FixingInformation):Unit = {
//    if (variables.size == 1) f.getDecimal(variables.head) match {
//      case Some(ff) => assignFixings(Map(variables.head -> ff))
//      case _ => {}
//    }
//  }
	
  def clearFixings = preFixings = UnderlyingFixing.empty

  def setFutureFixing = futureFixing = true

  def setPastFixing = futureFixing = false

  def getFixings:UnderlyingFixing = preFixings

  //def getDoubleFixings:Map[String, Double] = preFixings.getDouble

  def isFutureFixing:Boolean = futureFixing
	
  def isFixed = variables.isEmpty || (!preFixings.isEmpty && !isFutureFixing)

  protected var settlementFixings:UnderlyingFixing = UnderlyingFixing.empty // used for physical settlement only
  
  def isSettlementFixed:Boolean = true
  
  def assignSettlementFixings(f:UnderlyingFixing):Unit = {
    if ((variables subsetOf f.keySet) || f.isEmpty) settlementFixings = f
  }

//  def assignSettlementFixings(f:UnderlyingFixing)(implicit fixingInfo:FixingInformation):Unit = assignSettlementFixings(f.getDecimal)
//
//  def assignSettlementFixings(f:Double)(implicit fixingInfo:FixingInformation):Unit = {
//    if (variables.size == 1) f.getDecimal(variables.head) match {
//      case Some(ff) => assignSettlementFixings(Map(variables.head -> ff))
//      case _ => {}
//    }
//  }

  def clearSettlementFixings = settlementFixings = UnderlyingFixing.empty
    
  def getSettlementFixings = settlementFixings
  
}


trait FixingLegs[T <: FixingLeg] {
  
  val fixinglegs:LinearSeq[T]
  
  def assignFixings(fixings:List[UnderlyingFixing]):Unit = {
    assert(fixings.size == fixinglegs.size)
    (fixinglegs, fixings).zipped.foreach{case (p, f) => p.assignFixings(f)}
  }
	
//  def assignFixings(fixings:List[Option[Double]]) (implicit fixingInfo:FixingInformation):Unit = {
//    assert(fixings.size == fixinglegs.size)
//    (fixinglegs, fixings).zipped.foreach{
//      case (p, Some(f)) => p.assignFixings(f)
//      case (p, None) => {}}
//    }
	
  def isFixed:Boolean = fixinglegs.forall(_.isFixed)
  
  def assignSettlementFixings(fixings:List[UnderlyingFixing]):Unit = {
    assert(fixings.size == fixinglegs.size)
    (fixinglegs, fixings).zipped.foreach{case (p, f) => p.assignSettlementFixings(f)}
  }
    
//  def assignSettlementFixings(fixings:List[Option[Double]]) (implicit fixingInfo:FixingInformation):Unit = {
//    assert(fixings.size == fixinglegs.size)
//    (fixinglegs, fixings).zipped.foreach{
//      case (p, Some(f)) => p.assignSettlementFixings(f)
//      case (p, None) => {}}
//    }
  
}

