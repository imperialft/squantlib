package net.squantlib.math.financial

object Duration {
  
	/*	
	 * Returns Macaulay duration
	 */
  
	def macaulay(cashflows:List[(Double, Double)], discounter:Double => Double):Double = {
	  val (yearfrac, price) = cashflows.map{case (date, amount) => (date, amount * discounter(date))}.unzip
	  (yearfrac, price).zipped.map(_ * _).sum / price.sum
	}
	
	
	/*	
	 * Returns modified duration defined as Macauley duration / (1 + yield / freq)
	 */
	
	def modifiedContinuous(cashflows:List[(Double, Double)], discounter:Double => Double):Double = macaulay(cashflows, discounter)
	
	def modifiedCompounded(cashflows:List[(Double, Double)], discounter:Double => Double, freq:Int, bondYield:Double):Double = 
	  macaulay(cashflows, discounter) / (1.0 + bondYield / freq.toDouble)
	  

    def convexity(cashflows:List[(Double, Double)], discounter:Double => Double, f:(Double, Double, Double) => Double):Option[Double] = {
	  val (p, d2Pdy2) = cashflows.map{case (payday, amt) => (amt * discounter(payday), f(payday, amt, discounter(payday)))}.unzip
	  if (p.sum == 0) None else Some(d2Pdy2.sum / p.sum)
	}

	
	def convexitySimple(cashflows:List[(Double, Double)], discounter:Double => Double):Option[Double] = 
	  convexity(cashflows, discounter, (t:Double, c:Double, B:Double) => c * 2.0 * B * B * B * t * t)
	  
	def convexityCompounded(cashflows:List[(Double, Double)], discounter:Double => Double, freq:Int, currentRate:Double):Option[Double] = {
	  val f = freq.toDouble
	  convexity(cashflows, discounter, (t:Double, c:Double, B:Double) => c * B * t * (f * t + 1) / (f * (1 + currentRate / f) * (1 + currentRate / f)))
	}
	
	def convexityContinuous(cashflows:List[(Double, Double)], discounter:Double => Double):Option[Double] = 
	  convexity(cashflows, discounter, (t:Double, c:Double, B:Double) => c * B * t * t)
	
}

