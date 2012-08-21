
/**
 * Creates factory from given paramset.
 * You must define following parameters in advance.
 *  val paramset:String => parameter id
 */

import org.apache.commons.math3.distribution.NormalDistribution
import squantlib.math.random._
import squantlib.math.montecarlo.MonteCarlo_BS


val spot = 100.0
val ratedom = (d:Double) => 0.02 + d * 0.01
val ratefor = (d:Double) => 0.10 - d * 0.01
val sigma = (d:Double) => 0.10 + d * 0.10
val normdist = new NormalDistribution
//val NormSInv = (d:Double) => normdist.inverseCumulativeProbability(d)
val NormSInv = (d:Double) => MonteCarlo_BS.NormSInv(d)
val NormSDist = (d:Double) => normdist.cumulativeProbability(d)
val eventdates = Array(1.0, 2.0, 3.0)
val paydates = Array(1.0, 2.0, 3.0)
val strike = 100.0
val flow = Array.fill(3)((d:Double) => if (d > strike) d - strike else 0.0)
val discount = (d:Double) => math.exp(-ratedom(d) * d)
val paths = 1000000

println("Van der Corput")
var t0 = System.nanoTime
val generator_c = new CorputBase2_NR(1)
val rand_c = () => generator_c.sample
val mcresult_c = MonteCarlo_BS.MCPrice(spot, ratedom, ratefor, sigma, NormSInv, rand_c, eventdates, paydates, flow, discount, paths)
println("result: \n" + mcresult_c)
println("%-27.27s %.3f sec".format("Process time:", ((System.nanoTime - t0)/1000000000.0)))

println("Mersenne Twister")
t0 = System.nanoTime
val generator_mt = new MersenneTwister(1)
val rand_mt = () => generator_mt.sample
val mcresult_mt = MonteCarlo_BS.MCPrice(spot, ratedom, ratefor, sigma, NormSInv, rand_mt, eventdates, paydates, flow, discount, paths)
println("result: \n" + mcresult_mt)
println("%-27.27s %.3f sec".format("Process time:", ((System.nanoTime - t0)/1000000000.0)))

println("Mersenne Twister with inverse")
t0 = System.nanoTime
val generator_mt2 = new MersenneTwister(1)
val rand_mt2 = () => generator_mt2.sample
val mcresult_mt2 = MonteCarlo_BS.MCPrice2(spot, ratedom, ratefor, sigma, NormSInv, rand_mt2, eventdates, paydates, flow, discount, paths/2)
println("result: \n" + mcresult_mt2)
println("%-27.27s %.3f sec".format("Process time:", ((System.nanoTime - t0)/1000000000.0)))

println("Check Black Scholes:")
val pricebs = (0 to eventdates.size-1).map{ i => {
  val event = eventdates(i)
  val price = MonteCarlo_BS.blackScholes(spot, ratedom(event), ratefor(event), sigma(event), NormSDist, strike, event, discount(event))
  println("i = " + i + " => bs price = " + price)
}}


