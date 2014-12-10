package net.squantlib.schedule

import scala.collection.LinearSeq

trait FixingLeg {
  
  protected var preFixings:Map[String, Double] = Map.empty
	
  val variables:Set[String] // to be implemented
	
  def assignFixings(f:Map[String, Double]):Unit = 
    if ((variables subsetOf f.keySet) || f.isEmpty) preFixings = f
  
	
  def assignFixings(f:Double):Unit = if (variables.size == 1) assignFixings(Map(variables.head -> f))
	
  def clearFixings = preFixings = Map.empty
	
  def getFixings = preFixings
	
  def isFixed = variables.isEmpty || !preFixings.isEmpty
  
}


trait FixingLegs[T <: FixingLeg] {
  
  val fixinglegs:LinearSeq[T]
  
  def assignFixings(fixings:List[Map[String, Double]]):Unit = {
    assert(fixings.size == fixinglegs.size)
    (fixinglegs, fixings).zipped.foreach{case (p, f) => p.assignFixings(f)}
  }
	
  def assignFixings(fixings:List[Option[Double]]) (implicit d:DummyImplicit):Unit = {
    assert(fixings.size == fixinglegs.size)
    (fixinglegs, fixings).zipped.foreach{
      case (p, Some(f)) => p.assignFixings(f)
      case (p, None) => {}}
    }
	
  def isFixed:Boolean = fixinglegs.forall(_.isFixed)
  
}

