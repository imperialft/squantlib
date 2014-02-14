package squantlib.util

import DisplayUtils._

case class FixingInformation(current:Option[Double], minRange:Option[Double], maxRange:Option[Double], initialFixing:Map[String, Double]) {
    
  def all:Map[String, Double] = current match {
    case Some(c) => initialFixing.updated("tbd", c)
    case None => initialFixing
  }
    
  def update(p:String):String = multipleReplace(p, all.map{case (k, v) => ("@" + k, v)})
  
  private def multipleReplace(v:String, replacements:Map[String, Any]):String = {
	if (v == null) {return null}
	var result = v
	replacements.foreach{case (k, d) => result = result.replace(k, d.toString)}
	result
  }
  
  def updateCompute(p:String):Option[Double] = FormulaParser.calculate(update(p))
  
  /*
   * Fixing Information Accessor
   */
    
  def currentPercent(decimal:Int):String = current.collect{case v => v.asPercent(decimal)}.getOrElse("未定")
    
  def currentDouble(decimal:Int):String = current.collect{case v => v.asDouble(decimal)}.getOrElse("未定")
  
  def minRangePercent(decimal:Int):String = minRange.collect{case v => v.asPercent(decimal)}.getOrElse("未定")
    
  def minRangeDouble(decimal:Int):String = minRange.collect{case v => v.asDouble(decimal)}.getOrElse("未定")
    
  def maxRangePercent(decimal:Int):String = maxRange.collect{case v => v.asPercent(decimal)}.getOrElse("未定")
    
  def maxRangeDouble(decimal:Int):String = maxRange.collect{case v => v.asDouble(decimal)}.getOrElse("未定")
    
  def rangePercent(decimal:Int):String = (minRange, maxRange) match {
    case (min, max) if min.isDefined || max.isDefined => s"[${min.collect{case v => v.asPercent(decimal)}.getOrElse("")}～${max.collect{case v => v.asPercent(decimal)}.getOrElse("")}]"
    case _ => ""
  }
    
  def rangeDouble(decimal:Int):String = (minRange, maxRange) match {
    case (min, max) if min.isDefined || max.isDefined => s"[${min.collect{case v => v.asDouble(decimal)}.getOrElse("")}～${max.collect{case v => v.asDouble(decimal)}.getOrElse("")}]"
    case _ => ""
  }
    
}
  
object FixingInformation {
  
  def empty = FixingInformation(None, None, None, Map.empty)
  
}
