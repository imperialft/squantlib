package net.squantlib.math.timeseries

import scala.collection.immutable.SortedMap
import scala.collection.immutable.TreeMap

object Covariance{
  
  def calculate(x:Seq[Double], y:Seq[Double]):Double = {
    val elems = x.size.toDouble
    val mult = (x, y).zipped.map(_ * _).sum
    val ax = x.sum
    val ay = y.sum
    (mult - ax * ay / elems) / (elems - 1.0)
  }
  
    
}