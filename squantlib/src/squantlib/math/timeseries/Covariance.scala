package squantlib.math.timeseries

import scala.collection.immutable.SortedMap
import scala.collection.immutable.TreeMap
import org.jquantlib.time.{Date => qlDate}

object Covariance{
  
    def calculate(quotes1:IndexedSeq[Double], quotes2:IndexedSeq[Double]) : Double = {
		require (quotes1.size == quotes2.size)
        val datacount = quotes1.size
        val sumlog1 = quotes1.sum
        val sumlog2 = quotes2.sum
        val summult = ((0 to datacount-1) map (i => quotes1(i) * quotes2(i))).sum
        summult / datacount - sumlog1 / datacount * sumlog2 / datacount
    }

}