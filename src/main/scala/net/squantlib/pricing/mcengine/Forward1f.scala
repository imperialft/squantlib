package net.squantlib.pricing.mcengine

import net.squantlib.math.random.RandomGenerator
import net.squantlib.model.asset.Underlying
import net.squantlib.util.DisplayUtils._

/* Simple Black-Scholes montecarlo pricer for FX
 * FX volatility is constant over time without smile, No rates volatility
 * @param spot 		current underlying price
 * @param rate(t)	continuous compounding risk-free rate of pricing currency at time t as number of years
 * @param dividendYield(t)	continuous compounding dividend yield at time t as number of years
 * @param sigma(t)	volatility of the underlying FX
 */

case class Forward1f(
    var forward:Double => Double)
    extends Montecarlo1f {
  
  def getRandomGenerator:RandomGenerator = null
  def reset:Unit = {}
  
  /* Generates FX paths.
   * @param eventdates	FX observation dates as number of years
   * @param paths 	Number of Montecarlo paths to be generated
   * @returns Montecarlo paths
   * 
   * CAUTION: Order of event dates are automatically sorted by the function.
   * ie. Order of output paths might not correspond to order of input eventDates if it's not sorted.
  */
  
  def generatePaths(eventDates:List[Double], paths:Int, payoff:List[Double] => List[Double]):(List[Double], List[List[Double]]) = {
    require(!eventDates.isEmpty)
    
    val dates = eventDates.sorted
    val apath = dates.map(forward)
    (dates, List.fill(paths)(apath))
  }  

  override def modelName = this.getClass.toString
  
  override def spotref = List(forward(0.0))
  
  override def scheduledDescription = {
    val dates:List[Double] = (for(i <- 1 to 120 if (i <= 12 && i % 3 == 0)|| i % 12 == 0) yield i.toDouble / 12.0).toList
	
	val title = List("valuedate", "forward")
	val schedule = dates.map(d => {
	  List(d, forward(d)).map(_.asDouble)
	})
	
    (title, schedule)
  }  
}


object Forward1f {
  
	def apply(ul:Underlying):Option[Forward1f] = Some(new Forward1f(ul.forwardY))
	
	def apply(spot:Double, rate: Double => Double, dividendYield: Double => Double):Option[Forward1f] = {
	  val fwdfunc = (y:Double) => spot * math.exp(rate(y) * y) / math.exp(dividendYield(y) * y) 
	  Some(new Forward1f(fwdfunc))
	}
	
	
}