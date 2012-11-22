package squantlib.montecarlo.model

import squantlib.montecarlo.randomgenerator.{RandomGenerator, MersenneTwister}
import squantlib.math.statistical.NormSInv


/* Simple Black-Scholes montecarlo pricer for FX
 * FX volatility is constant over time without smile, No rates volatility
 * @param spot 		current underlying price
 * @param ratedom(t)	continuous compounding risk-free rate of domestic pricing currency at time t as number of years
 * @param ratefor(t)	continuous compounding risk-free rate of foreign currency at time t as number of years
 * @param sigma(t)	volatility of the underlying FX
 */

case class FXBlackScholes1f(var spot:Double, var ratedomF: Double => Double, var rateforF: Double => Double, var sigmaF: Double => Double) {
  
  var normSInv: Double => Double = (x:Double) => NormSInv(x)
  
  var randomGenerator:RandomGenerator = new MersenneTwister(1)
  
  def reset = randomGenerator = new MersenneTwister(1)

  /* Generates FX paths.
   * @param eventdates	FX observation dates as number of years
   * @param paths 	Number of Montecarlo paths to be generated
   * @returns Montecarlo paths
   * 
   * CAUTION: Order of event dates are automatically sorted by the function.
   * ie. Order of output paths might not correspond to order of input eventDates if it's not sorted.
  */
  
  def generatePaths(eventDates:List[Double], paths:Int):List[List[Double]] = {
    require(!eventDates.isEmpty)
    
    val dates = eventDates.sorted
    val steps = dates.size
    val stepsize = dates.head :: (dates.tail, dates).zipped.map(_ - _)
    				
    val ratedom = dates.map(ratedomF)
    val ratefor = dates.map(rateforF)
    val sigma = dates.map(sigmaF)
    
    val fratedom = ratedom.head :: (for (i <- (1 to steps-1).toList) yield ((ratedom(i) * dates(i) - ratedom(i-1) * dates(i-1)) / stepsize(i)))
    val fratefor = ratefor.head :: (for (i <- (1 to steps-1).toList) yield ((ratefor(i) * dates(i) - ratefor(i-1) * dates(i-1)) / stepsize(i)))
    val fvol = sigma.head :: (for (i <- (1 to steps-1).toList) yield math.sqrt((dates(i) * sigma(i) * sigma(i) - dates(i-1) * sigma(i-1) * sigma(i-1)) / stepsize(i)))
    
	val drift = for (i <- 0 to steps-1) yield (fratedom(i) - fratefor(i) - ((fvol(i) * fvol(i)) / 2)) * stepsize(i)
	val sigt = for (i <- 0 to steps-1) yield fvol(i) * scala.math.sqrt(stepsize(i))
	
    for (path <- (0 to paths-1).toList) yield {
      var spotprice = spot
      for (d <- (0 to steps-1).toList) yield {
        val rnd = randomGenerator.sample
		val ninv1 = normSInv(rnd)
		spotprice *= scala.math.exp(drift(d) + (sigt(d) * ninv1))
		spotprice
      }
    }
  }  

}

