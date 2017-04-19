package net.squantlib.util

import net.squantlib.util.DisplayUtils._
import scala.annotation.tailrec

class MultiOrderObject[T](
  firstOrder:T,
  secondOrder:T,
  thirdOrder:T
)

case class MultiOrderMap(
  firstOrder: Map[String, Option[Double]],
  secondOrder: Map[String, Option[Double]] = Map.empty,
  thirdOrder: Map[String, Option[Double]] = Map.empty
) extends MultiOrderObject[Map[String, Option[Double]]](firstOrder, secondOrder, thirdOrder) {
  
  private def optionMapToDouble(m:Map[String, Option[Double]]):Map[String, Double] = m.collect{case (a, Some(b)) => (a -> b)}
  
  def firstOrderMap = optionMapToDouble(firstOrder)

  def secondOrderMap = optionMapToDouble(secondOrder)
 
  def thirdOrderMap = optionMapToDouble(thirdOrder)
  
  override def toString = List(("1", firstOrderMap), ("2", secondOrderMap), ("3", thirdOrderMap))
    .filter{case (k, v) => !v.isEmpty}
    .map{case (k, vs) => s"<${k}> ${vs.map{case (kk, vv) => kk + ":" + vv.asPercent(5)}.mkString(" ")}"}.mkString(" ")
  
}

object MultiOrderMap {
  
  def apply(a:Iterable[(String, MultiOrderNumber)]):MultiOrderMap = {
    def firstOrder:Map[String, Option[Double]] = a.map{case (k, v) => (k -> v.firstOrder)}(collection.breakOut)
    def secondOrder:Map[String, Option[Double]] = a.map{case (k, v) => (k -> v.secondOrder)}(collection.breakOut)
    def thirdOrder:Map[String, Option[Double]] = a.map{case (k, v) => (k -> v.thirdOrder)}(collection.breakOut)
    MultiOrderMap(firstOrder, secondOrder, thirdOrder)
  }
  
  def empty = MultiOrderMap(Map.empty)
  
}

case class MultiOrderNumber(
  firstOrder: Option[Double],
  secondOrder: Option[Double] = None,
  thirdOrder: Option[Double] = None
) extends MultiOrderObject[Option[Double]](firstOrder, secondOrder, thirdOrder) 

object MultiOrderNumber {
  def empty = MultiOrderNumber(None, None, None)
  
  def priceToHigherOrder(prices:Iterable[(Double, Double)]):Set[MultiOrderNumber] = {
    if (prices.size < 2) {return Set.empty}

    val priceList:List[(Double, Double)] = prices.toList.sortBy{case (k, v) => k}
    
//    @tailrec def computeGradient(ls:List[(Double, Double)], acc:List[(Double, Double)]) = ls match {
//      case Nil => acc.reverse
//      case (k, v)::t if t.isEmpty => 
//    }
    null
    
  }

}