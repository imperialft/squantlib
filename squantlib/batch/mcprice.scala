
/**
 * Creates factory from given paramset.
 * You must define following parameters in advance.
 *  val paramset:String => parameter id
 */

import org.apache.commons.math3.distribution.NormalDistribution
import squantlib.math.random._
import squantlib.math.montecarlo.MonteCarlo_BS


val spot = 100.0
val ratedom = (d:Double) => 0.0
val ratefor = (d:Double) => 0.0
val sigma = (d:Double) => 0.35
val normdist = new NormalDistribution
//val NormSInv = (d:Double) => normdist.inverseCumulativeProbability(d)
val NormSInv = (d:Double) => MonteCarlo_BS.NormSInv(d)
val NormSDist = (d:Double) => normdist.cumulativeProbability(d)
//val generator = new CorputBase2_NR(1) // produce false result
val generator = new MersenneTwister(1)
val rand = () => generator.sample
val eventdates = Array(1.0, 2.0, 3.0)
val paydates = Array(1.0, 2.0, 3.0)
val strike = 100.0
val flow = Array.fill(3)((d:Double) => if (d > strike) d - strike else 0.0)
val discount = (d:Double) => 1.00
val paths = 100000

val t0 = System.nanoTime

val mcresult = MonteCarlo_BS.MCPrice(spot, ratedom, ratefor, sigma, NormSInv, rand, eventdates, paydates, flow, discount, paths)
println("result: \n" + mcresult)

println("Check Black Scholes:")
val pricebs = (0 to eventdates.size-1).map{ i => {
  val event = eventdates(i)
  val price = MonteCarlo_BS.blackScholes(spot, ratedom(event), ratefor(event), sigma(event), NormSDist, strike, event, discount(event))
  println("i = " + i + " => bs price = " + price)
}}


println("%-27.27s %.3f sec".format("Process time:", ((System.nanoTime - t0)/1000000000.0)))

