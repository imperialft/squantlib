package net.squantlib.util

import scala.annotation.tailrec
import DisplayUtils._

case class FixingInformation(
    var tbd:Option[Double], 
    var minRange:Option[Double], 
    var maxRange:Option[Double], 
    var initialFixing:Map[String, Double]) {
    
  def all:Map[String, Double] = tbd match {
    case Some(c) => initialFixing.updated("tbd", c)
    case None => initialFixing
  }
    
  def update(p:String):String = multipleReplace(p, all.map{case (k, v) => ("@" + k, v)})
  
  def updateInitial(p:String):String = multipleReplace(p, initialFixing.map{case (k, v) => ("@" + k, v)})
  
  @tailrec private def multipleReplace(s:String, replacements:Map[String, Double]):String = 
    if (s == null) null
    else replacements.headOption match {
      case None => s
      case Some((k, v)) => multipleReplace(s.replace(k, v.toString), replacements - k)
    }
  
  def updateCompute(p:String):Option[Double] = FormulaParser.calculate(update(p))
  
  /*
   * Fixing Information Accessor
   */
    
  def currentPercent(decimal:Int):String = tbd.collect{case v => v.asPercent(decimal)}.getOrElse("未定")
    
  def currentDouble(decimal:Int):String = tbd.collect{case v => v.asDouble(decimal)}.getOrElse("未定")
  
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
