package net.squantlib.math.timeseries

import scala.collection.SortedMap
import net.squantlib.util.Date

object Correlation {
  
  def calculate(quotes1:IndexedSeq[Double], quotes2:IndexedSeq[Double], nbData:Int):IndexedSeq[Double] = {
    require (quotes1.size == quotes2.size)
    if ((quotes1, quotes2).zipped.map((a, b) => math.abs(a - b)).sum < 0.0000001) {return IndexedSeq.fill(quotes1.size - nbData + 1)(1.0)}
    
    val logset1 = LogReturn.calculate(quotes1)
    val logset2 = LogReturn.calculate(quotes2)
    val datacount = nbData - 1
      
    (datacount to logset1.size).map( i => { 
  	val startdate = i - datacount
  	val enddate = i
  	val q1:Array[Double] = logset1.slice(startdate, enddate)
  	val q2:Array[Double] = logset2.slice(startdate, enddate)
  	  
  	if (q1.forall(_ - 0.0 < 0.00000001) || q2.forall(_ - 0.0 < 0.00000001)) 0.0 
  	else {
      val meanx = q1.sum / q1.size.toDouble
      val meany = q2.sum / q2.size.toDouble
      
      def dx(x:Double) = x - meanx
      def dy(y:Double) = y - meany
      
      val cov = (q1, q2).zipped.map{case (x, y) => dx(x) * dy(y)}.sum
      val v = q1.map(x => dx(x) * dx(x)).sum * q2.map(y => dy(y) * dy(y)).sum
      cov / Math.sqrt(v)
//  	  Covariance.calculate(q1, q2) / StdDev.calculate(q1.toSet) / StdDev.calculate(q2.toSet)

  	}
    
    }) (collection.breakOut)}
  
  def calculate(quotes:IndexedSeq[(Double, Double)], nbData:Int):IndexedSeq[Double] = quotes.unzip match {
    case (q1, q2) => calculate(q1, q2, nbData)
  }
  
  def calculate(quotes1:IndexedSeq[Double], quotes2:IndexedSeq[Double]):IndexedSeq[Double] = calculate(quotes1, quotes2, quotes1.size)
  
  def calculate(quotes:SortedMap[Date, (Double, Double)], nbData:Int):SortedMap[Date, Double] = {
    val correl = calculate(quotes.unzip._2.toIndexedSeq, nbData).toSeq
    SortedMap((quotes.unzip._1.takeRight(correl.size) zip correl)(collection.breakOut) :_*)
  }
  
}

