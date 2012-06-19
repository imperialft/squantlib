package squantlib.math.timeseries
 
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap
import org.jquantlib.time.{ Date => JDate }

object LogReturn {
	/**
	 * Returns daily log return for given period.
	 * @return array of size n-1 containing ln(Xn / Xn-1)
	 */
	def calculate(values:Array[Double]) : Array[Double] = {
	  (1 to (values.size-1)) map (i => math.log(values(i) / values(i-1))) toArray
	}
  
	/**
	 * Returns daily log return for given period.
	 * @return array of size n-1 containing ln(Xn / Xn-1)
	 */
	def calculate(values: SortedMap[JDate, Double]) : SortedMap[JDate, Double] = {
	  val keys = values.keySet.toIndexedSeq
	  TreeMap(((1 to (values.size-1)) map (i => (keys(i), math.log(values(keys(i)) / values(keys(i-1)))))) :_*)
	}
  
}