package squantlib.pricing.mcengine

import squantlib.pricing.randomgenerator.RandomGenerator
import squantlib.model.fx.FX

/* Simple Black-Scholes montecarlo pricer for FX
 * FX volatility is constant over time without smile, No rates volatility
 * @param spot 		current underlying price
 * @param ratedom(t)	continuous compounding risk-free rate of domestic pricing currency at time t as number of years
 * @param ratefor(t)	continuous compounding risk-free rate of foreign currency at time t as number of years
 * @param sigma(t)	volatility of the underlying FX
 */

case class FXzeroVol1f(var spot:Double, var zcDomF: Double => Double, var zcForF: Double => Double) extends Montecarlo1f {
  
  var randomGenerator:RandomGenerator = null
  def reset:Unit = {}
  
  /* Generates FX paths.
   * @param eventdates	FX observation dates as number of years
   * @param paths 	Number of Montecarlo paths to be generated
   * @returns Montecarlo paths
   * 
   * CAUTION: Order of event dates are automatically sorted by the function.
   * ie. Order of output paths might not correspond to order of input eventDates if it's not sorted.
  */
  
  def generatePaths(eventDates:List[Double], paths:Int):(List[Double], List[List[Double]]) = {
    require(!eventDates.isEmpty)
    
    val dates = eventDates.sorted
    val apath = dates.map(d => spot * zcForF(d) / zcDomF(d))
    (dates, List.fill(paths)(apath))
  }  

}


object FXzeroVol1f {
  
	def apply(fx:FX):Option[FXzeroVol1f] = Some(new FXzeroVol1f(fx.spot, fx.zcDomY, fx.zcForY))
}