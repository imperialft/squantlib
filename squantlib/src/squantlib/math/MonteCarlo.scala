package squantlib.math.montecarlo

import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.stat.descriptive.moment.StandardDeviation
import squantlib.math.random._

object MonteCarlo_BS{
  
  /* One factor, constant volatility, one payment
   */
  def simpleVanilla(spot:Double, ratedom:Double, ratefor:Double, sigma:Double, NormSInv:Double => Double, 
      Rand:()=>Double, Flow:Double => Double, time:Double, discount:Double, paths:Int):(Double, Double) = {
    
    val rndrift = (ratedom - ratefor - ((sigma * sigma) / 2)) * time
    val sigt = sigma * scala.math.sqrt(time)
    val stddev = new StandardDeviation
     
    val mcresult = (0 to paths - 1).map{i => {
        val rnd = Rand()
        val ninv = NormSInv(rnd)
        val underlying = spot * scala.math.exp(rndrift + (sigt * ninv))
        Flow(underlying)
     }}
    
    (mcresult.sum / paths * discount, stddev.evaluate(mcresult.toArray) / math.sqrt(paths))
  }
  
  def blackScholes(spot:Double, ratedom:Double, ratefor:Double, sigma:Double, NormSInv:Double => Double, 
      strike:Double, time:Double, discount:Double):Double = {

    val h1 = (math.log(spot / strike) + ((ratedom - ratefor) + sigma * sigma / 2) * time) / (sigma * math.sqrt(time))
    val h2 = h1 - sigma * math.sqrt(time)
    spot * math.exp(-ratefor * time) * NormSInv(h1) - strike * math.exp(-ratedom * time) * NormSInv(h2)
  }
}