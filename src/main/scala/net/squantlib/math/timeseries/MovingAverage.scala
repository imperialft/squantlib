package net.squantlib.math.timeseries

import scala.language.postfixOps
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap
import net.squantlib.util.Date

object MovingAverage {
	/**
	 * Returns daily log return for given period.
	 * @return array of size n-1 containing ln(Xn / Xn-1)
	 */
	def calculate(values:Array[Double], nbDays:Int) : Array[Double] = {
	  require(values.size >= nbDays)
	  (nbDays to values.size) map (i => ((i-nbDays) to (i-1)).map(j => values(j)).sum / nbDays) toArray
	}
  
	/**
	 * Returns daily log return for given period.
	 * @return array of size n-1 containing ln(Xn / Xn-1)
	 */
	def calculate(values: SortedMap[Date, Double], nbDays:Int) : SortedMap[Date, Double] = {
	  val keys = values.keySet.toIndexedSeq
	  TreeMap((nbDays to values.size) map (i => (keys(i-1), ((i-nbDays) to (i-1)).map(j => values(keys(j))).sum / nbDays)) :_*)
	}
  
}