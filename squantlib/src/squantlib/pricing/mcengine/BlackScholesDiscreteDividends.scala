package squantlib.pricing.mcengine

import squantlib.math.random.{RandomGenerator, MersenneTwister}
import squantlib.math.statistical.NormSInv
import squantlib.model.index.Index
import squantlib.model.index.DiscreteIndex


/* Black-Scholes montecarlo generator with discrete dividends
 * - Discrete dividend
 * - Volatility is constant over time without smile
 * - No rate & dividend volatility
 * @param spot 		current underlying price
 * @param rate(t)	continuous compounding risk-free rate of pricing currency at time t as number of years
 * @param dividends	dividend schedule described as date(#year from value date) => amount
 * @param repo(t)	repo rate at time t as number of years
*  * @param sigma(t)	volatility of the underlying index
 */

case class BlackScholesDiscreteDividends(
    var spot:Double, 
    var rate: Double => Double, 
    var dividends: Map[Double, Double], 
    var repoYield: Double => Double, 
    var volatility: Double => Double) 
    extends Montecarlo1f{
  
  var normSInv: Double => Double = (x:Double) => NormSInv(x)
  
  override var randomGenerator:RandomGenerator = new MersenneTwister(1)
  
  override def reset = randomGenerator = new MersenneTwister(1)

  /* Generates paths.
   * @param eventdates	observation dates as number of years
   * @param paths 	Number of Montecarlo paths to be generated
   * @returns Montecarlo paths
   * 
   * CAUTION: Order of event dates are automatically sorted and duplicates removed by the function.
   * Check with first tuple argument for the order of dates.
  */
  
  override def generatePaths(eventDates:List[Double], paths:Int):(List[Double], List[List[Double]]) = {
//    if (eventDates.isEmpty) {return (List.empty, List.empty)}
//    
//    reset 
//    
//    val dates = eventDates.sorted
//    val steps = dates.size
//    val stepsize = dates.head :: (dates.tail, dates).zipped.map(_ - _)
//
//    val ratedom = dates.map(rate)
//    val ratefor = dates.map(d => dividendYield(d) + repoYield(d))
//    val sigma = dates.map(volatility)
//    
//    val fratedom = ratedom.head :: (for (i <- (1 to steps-1).toList) yield 
//        (if (stepsize(i) == 0.0) 0.0 else (ratedom(i) * dates(i) - ratedom(i-1) * dates(i-1)) / stepsize(i)))
//    
//    val fratefor = ratefor.head :: (for (i <- (1 to steps-1).toList) yield 
//        (if (stepsize(i) == 0.0) 0.0 else (ratefor(i) * dates(i) - ratefor(i-1) * dates(i-1)) / stepsize(i)))
//    
//    val fsigma = sigma.head :: (for (i <- (1 to steps-1).toList) yield 
//        (if (stepsize(i) == 0.0) 0.0 else math.sqrt((dates(i) * sigma(i) * sigma(i) - dates(i-1) * sigma(i-1) * sigma(i-1)) / stepsize(i))))
//    
//	val drift = for (i <- 0 to steps-1) yield (fratedom(i) - fratefor(i) - ((fsigma(i) * fsigma(i)) / 2.0)) * stepsize(i)
//	
//	val sigt = for (i <- 0 to steps-1) yield fsigma(i) * scala.math.sqrt(stepsize(i))
//	
//    val genpaths = for (path <- (0 to paths-1).toList) yield {
//      var spotprice = spot
//      for (d <- (0 to steps-1).toList) yield {
//        if (stepsize(d) == 0.0) spotprice
//        else {
//          val rnd = randomGenerator.sample
//          val ninv1 = normSInv(rnd)
//          spotprice *= scala.math.exp(drift(d) + (sigt(d) * ninv1))
//          spotprice
//          }
//      }
//    }
//    
//    (dates, genpaths)
    null
  }

}

object BlackScholesDiscreteDividends {
  
  def apply(index:DiscreteIndex):Option[BlackScholesDiscreteDividends] = 
	try { Some(new BlackScholesDiscreteDividends(index.spot, index.interestRate, index.dividendMapY, index.repoRate, index.volatilityY)) } 
	catch { case _ :Throwable=> None}
	
}

