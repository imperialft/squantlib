package squantlib.math.timeseries

import scala.collection.SortedMap
import scala.collection.immutable.TreeMap
import org.jquantlib.time.{ Date => JDate }

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
	def calculate(values: SortedMap[JDate, Double], nbDays:Int) : SortedMap[JDate, Double] = {
	  val keys = values.keySet.toIndexedSeq
	  TreeMap((nbDays to values.size) map (i => (keys(i-1), ((i-nbDays) to (i-1)).map(j => values(keys(j))).sum / nbDays)) :_*)
	}
  
}