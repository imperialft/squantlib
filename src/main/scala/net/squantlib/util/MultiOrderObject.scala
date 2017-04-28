package net.squantlib.util

import net.squantlib.util.DisplayUtils._
import scala.annotation.tailrec

class MultiOrderObject[T](
  spot:T,
  firstOrder:T,
  secondOrder:T,
  thirdOrder:T
)

case class MultiOrderMap(
  spot:Map[String, Option[Double]],
  firstOrder: Map[String, Option[Double]],
  secondOrder: Map[String, Option[Double]] = Map.empty,
  thirdOrder: Map[String, Option[Double]] = Map.empty,
  priceUp: Map[String, Option[Double]] = Map.empty,
  priceDown: Map[String, Option[Double]] = Map.empty
) extends MultiOrderObject[Map[String, Option[Double]]](spot, firstOrder, secondOrder, thirdOrder) {
  
  private def optionMapToDouble(m:Map[String, Option[Double]]):Map[String, Double] = m.collect{case (a, Some(b)) => (a -> b)}
  
  def spotMap = optionMapToDouble(spot)

  def firstOrderMap = optionMapToDouble(firstOrder)

  def secondOrderMap = optionMapToDouble(secondOrder)
 
  def thirdOrderMap = optionMapToDouble(thirdOrder)
  
  override def toString = List(("0", spotMap), ("1", firstOrderMap), ("2", secondOrderMap), ("3", thirdOrderMap))
    .filter{case (k, v) => !v.isEmpty}
    .map{case (k, vs) => s"${k}> ${vs.map{case (kk, vv) => kk + ":" + vv.asPercent(5)}.mkString(" ")}"}.mkString(" ")
  
}

object MultiOrderMap {
  
  def apply(a:Iterable[(String, MultiOrderNumber)]):MultiOrderMap = {
    def spot:Map[String, Option[Double]] = a.map{case (k, v) => (k -> v.spot)}(collection.breakOut)
    def firstOrder:Map[String, Option[Double]] = a.map{case (k, v) => (k -> v.firstOrder)}(collection.breakOut)
    def secondOrder:Map[String, Option[Double]] = a.map{case (k, v) => (k -> v.secondOrder)}(collection.breakOut)
    def thirdOrder:Map[String, Option[Double]] = a.map{case (k, v) => (k -> v.thirdOrder)}(collection.breakOut)
    def priceUp:Map[String, Option[Double]] = a.map{case (k, v) => (k -> v.priceUp)}(collection.breakOut)
    def priceDown:Map[String, Option[Double]] = a.map{case (k, v) => (k -> v.priceDown)}(collection.breakOut)
    MultiOrderMap(spot, firstOrder, secondOrder, thirdOrder, priceUp, priceDown)
  }
  
  def empty = MultiOrderMap(Map.empty, Map.empty)
  
}

case class MultiOrderMap2D(
  first: MultiOrderMap,
  second: MultiOrderMap,
  cross: MultiOrderMap
) {

  override def toString = List(("0", first), ("1", second), ("2", cross)).map{case (k, vs) => s"<${k}> ${vs.toString}"}.mkString(" ")
  
}

object MultiOrderMap2D {
  
  def empty = MultiOrderMap2D(MultiOrderMap.empty, MultiOrderMap.empty,MultiOrderMap.empty)
  
  def apply(a:Iterable[(String, MultiOrderNumber2D)]):MultiOrderMap2D = {
    val first:MultiOrderMap = MultiOrderMap(a.map{case (k, v) => (k -> v.first)}) 
    val second:MultiOrderMap = MultiOrderMap(a.map{case (k, v) => (k -> v.second)}) 
    val cross:MultiOrderMap = MultiOrderMap(a.map{case (k, v) => (k -> v.cross)}) 
    MultiOrderMap2D(first, second, cross)
  }
  
}

case class MultiOrderNumber(
  spot: Option[Double],
  firstOrder: Option[Double],
  secondOrder: Option[Double] = None,
  thirdOrder: Option[Double] = None,
  priceUp:Option[Double] = None,
  priceDown:Option[Double] = None
) extends MultiOrderObject[Option[Double]](spot, firstOrder, secondOrder, thirdOrder) 

object MultiOrderNumber {
  def empty = MultiOrderNumber(None, None, None, None)
  
  def priceToHigherOrder(prices:Iterable[(Double, Double)]):List[(Double, MultiOrderNumber)] = {
    if (prices.size <= 2) {return List.empty}

    val priceList:List[(Double, Double)] = prices.toList.sortBy{case (k, v) => k}
    val first = computeGradient(priceList, None, List.empty)
//    val simpleFirst = computeSimpleGradient(priceList, None, List.empty)
//    val second = computeGradient(simpleFirst, None, List.empty)
//    val simpleSecond = computeSimpleGradient(simpleFirst, None, List.empty)
//    val third = computeGradient(simpleSecond, None, List.empty)
    val second = computeGradient(first, None, List.empty)
    val third = computeGradient(second, None, List.empty)
    
    priceList.zip(first).zip(second.zip(third)).map{case (((k, n), (_, f)), ((_, s), (_, t))) => (k, MultiOrderNumber(Some(n), Some(f), Some(s), Some(t)))}.toList
  }

  @tailrec def computeGradient(ls:List[(Double, Double)], prev:Option[Double], acc:List[(Double, Double)]):List[(Double, Double)] = ls match {
    case Nil => acc.reverse
    case (k, v)::t if t.isEmpty => ((k, prev.getOrElse(0.0)) :: acc).reverse
    case (k1, v1) ::(k2, v2) :: t => 
      val current:Double = (v2 - v1) / (k2 - k1)
      val result:Double = prev.collect{case p => (current + p) / 2}.getOrElse(current)
     computeGradient((k2, v2)::t, Some(current), (k1, result) :: acc)
  }

//  @tailrec def computeSimpleGradient(ls:List[(Double, Double)], prev:Option[Double], acc:List[(Double, Double)]):List[(Double, Double)] = ls match {
//    case Nil => acc.reverse
//    case (k, v)::t if t.isEmpty => ((k, prev.getOrElse(0.0)) :: acc).reverse
//    case (k1, v1) ::(k2, v2) :: t => 
//      val current:Double = (v2 - v1) / (k2 - k1)
//     computeSimpleGradient((k2, v2)::t, Some(current), (k1, current) :: acc)
//  }
  
}

case class MultiOrderNumber2D (
  first: MultiOrderNumber,
  second: MultiOrderNumber,
  cross: MultiOrderNumber
)

object MultiOrderNumber2D {
  def empty = new MultiOrderNumber2D(MultiOrderNumber.empty, MultiOrderNumber.empty, MultiOrderNumber.empty)
}
