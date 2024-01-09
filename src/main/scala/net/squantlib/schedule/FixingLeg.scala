package net.squantlib.schedule

import net.squantlib.util.FixingInformation
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.UnderlyingFixing
import scala.collection.LinearSeq

trait FixingLeg {
  
  protected var preFixings:UnderlyingFixing = UnderlyingFixing.empty

  protected var overrideFixings:UnderlyingFixing = UnderlyingFixing.empty

  protected var isFixedByOverrideFixings:Boolean = false

  protected var futureFixing:Boolean = false

  val fixingInfo:FixingInformation

  val variables:Set[String] // to be implemented
	
  def assignFixings(f:UnderlyingFixing):Unit = {
    val mergedFixings:UnderlyingFixing = f.update(overrideFixings)
    if (mergedFixings.isValidFor(variables) || mergedFixings.isEmpty) preFixings = mergedFixings
  }

  def assignOverrideFixings(f:UnderlyingFixing):Unit = {
    overrideFixings = f.nonEmptyFixing
    isFixedByOverrideFixings = overrideFixings.isValidFor(variables)
  }

  def clearFixings = preFixings = UnderlyingFixing.empty

  def setFutureFixing = futureFixing = true

  def setPastFixing = futureFixing = false

  // def getFixings:UnderlyingFixing = preFixings
  def getFixings:UnderlyingFixing = preFixings.update(overrideFixings)

  def getOverrideFixings:UnderlyingFixing = overrideFixings

  def isFutureFixing:Boolean = futureFixing
	
  // def isFixed = variables.isEmpty || (!preFixings.isEmpty && !isFutureFixing)
  def isFixed = variables.isEmpty || (!preFixings.isEmpty && !isFutureFixing) || isFixedByOverrideFixings

  def isPastFixed = variables.isEmpty || ((!preFixings.isEmpty || isFixedByOverrideFixings) && !isFutureFixing)

  protected var settlementFixings:UnderlyingFixing = UnderlyingFixing.empty // used for physical settlement only
  
  def isSettlementFixed:Boolean = true
  
  def assignSettlementFixings(f:UnderlyingFixing):Unit = {
    if (f.isValidFor(variables) || f.isEmpty) settlementFixings = f
  }

  def clearSettlementFixings = settlementFixings = UnderlyingFixing.empty
    
  def getSettlementFixings = settlementFixings.update(overrideFixings)
  
}


trait FixingLegs[T <: FixingLeg] {
  
  val fixinglegs: LinearSeq[T]
  
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

