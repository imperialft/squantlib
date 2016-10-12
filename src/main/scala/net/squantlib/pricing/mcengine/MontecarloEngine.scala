package net.squantlib.pricing.mcengine

import net.squantlib.math.random.RandomGenerator
import net.squantlib.math.statistical.NormSInv
import net.squantlib.util.DisplayUtils._

trait MontecarloEngine {
  
  def modelName:String
  
  def spotref:List[Double]
  
  def scheduledDescription:(List[String], List[List[String]])
  
}

/* Simple Black-Scholes montecarlo pricer for FX
 * FX volatility is constant over time without smile, No rates volatility
 * @param spot 		current underlying price
 * @param ratedom(t)	continuous compounding risk-free rate of domestic pricing currency at time t as number of years
 * @param ratefor(t)	continuous compounding risk-free rate of foreign currency at time t as number of years
 * @param sigma(t)	volatility of the underlying FX
 */

trait Montecarlo1f extends MontecarloEngine {
  
  def getRandomGenerator:RandomGenerator
  
  /* Generates paths.
   * @param eventdates observation dates as number of years
   * @param paths 	Number of Montecarlo paths to be generated
   * @returns Montecarlo paths
   * 
   * CAUTION: Order of event dates are automatically sorted by the function.
   * ie. Order of output paths might not correspond to order of input eventDates if it's not sorted.
  */
  
  def generatePaths(eventDates:List[Double], paths:Int, payoff:List[Double] => List[Double]):(List[Double], List[List[Double]])
  
  def analyzePaths(dates:List[Double], paths:List[List[Double]]):Unit = {
    val average = paths.transpose.map(_.sum).map(_ / paths.size)
    average.foreach (a => standardOutput(a))
  }
  
}

trait MontecarloNf extends MontecarloEngine {
  
  def getRandomGenerator:RandomGenerator
  
  /* Generates paths.
   * @param eventdates observation dates as number of years
   * @param paths 	Number of Montecarlo paths to be generated
   * @returns Montecarlo paths
   * 
   * CAUTION: Order of event dates are automatically sorted by the function.
   * ie. Order of output paths might not correspond to order of input eventDates if it's not sorted.
  */
  
  def generatePaths[A](
      eventDates:List[Double], 
      paths:Int, 
      payoff:List[Map[String, Double]] => List[A]):(List[Double], List[List[A]])
  
  def analyzePaths(dates:List[Double], paths:List[List[Double]]):Unit = {
    val average = paths.transpose.map(_.sum).map(_ / paths.size)
    average.foreach (a => standardOutput(a))
  }
  
}
