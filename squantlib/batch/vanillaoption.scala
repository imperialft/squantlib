
/**
 * Creates factory from given paramset.
 * You must define following parameters in advance.
 *  val paramset:String => parameter id
 */

import org.apache.commons.math3.distribution.NormalDistribution
import squantlib.math.random._
import squantlib.math.montecarlo.MonteCarlo_BS

val spot = 100.0
val ratedom = 0.04
val ratefor = 0.08
val sigma = 0.35
val normdist = new NormalDistribution
val NormSInv = (d:Double) => normdist.inverseCumulativeProbability(d)
val NormSDist = (d:Double) => normdist.cumulativeProbability(d)
val strike = 100.0
val flow = (d:Double) => if (d > strike) d - strike else 0.0
val time = 3.0
//val discount = math.exp(-ratedom * time)
val discount = 1.0
val paths = List(1000, 5000, 10000, 30000, 50000, 100000, 200000)

val pricebs = MonteCarlo_BS.blackScholes(spot, ratedom, ratefor, sigma, NormSDist, strike, time, discount)
println("Black Scholes = " + pricebs)

var t0 = System.nanoTime

println("Mersenne Twister")
val mtgenerator = new MersenneTwister(29817393471L)
val mtrand = () => mtgenerator.sample
paths.foreach(p => {
	val price = MonteCarlo_BS.simpleVanilla(spot, ratedom, ratefor, sigma, NormSInv, mtrand, flow, time, discount, p)
	println(p + " paths => " + price._1 + " stddev " + price._2)
})
println("%-27.27s %.3f sec".format("Process time:", ((System.nanoTime - t0)/1000000000.0)))

t0 = System.nanoTime

println("van der Corput sequence")
val cgenerator = new CorputBase2(0)
val crand = () => cgenerator.sample
paths.foreach(p => {
	val price = MonteCarlo_BS.simpleVanilla(spot, ratedom, ratefor, sigma, NormSInv, crand, flow, time, discount, p)
	println(p + " paths => " + price._1 + " stddev " + price._2)
})
println("%-27.27s %.3f sec".format("Process time:", ((System.nanoTime - t0)/1000000000.0)))

t0 = System.nanoTime

println("van der Corput sequence (non recursive)")
val nrgenerator = new CorputBase2_NR(1)
val nrrand = () => nrgenerator.sample
paths.foreach(p => {
	val price = MonteCarlo_BS.simpleVanilla(spot, ratedom, ratefor, sigma, NormSInv, nrrand, flow, time, discount, p)
	println(p + " paths => " + price._1 + " stddev " + price._2)
})
println("%-27.27s %.3f sec".format("Process time:", ((System.nanoTime - t0)/1000000000.0)))

t0 = System.nanoTime

println("java rangen")
val jgenerator = new Java(19754374513458L)
val jrand = () => jgenerator.sample
paths.foreach(p => {
	val price = MonteCarlo_BS.simpleVanilla(spot, ratedom, ratefor, sigma, NormSInv, jrand, flow, time, discount, p)
	println(p + " paths => " + price._1 + " stddev " + price._2)
})
println("%-27.27s %.3f sec".format("Process time:", ((System.nanoTime - t0)/1000000000.0)))

t0 = System.nanoTime

println("Well44497b")
val well44497b = new Well44497b(18927491241L)
val wbrand = () => well44497b.sample
paths.foreach(p => {
  val price = MonteCarlo_BS.simpleVanilla(spot, ratedom, ratefor, sigma, NormSInv, wbrand, flow, time, discount, p)
  println(p + " paths => " + price._1 + " stddev " + price._2)
})
println("%-27.27s %.3f sec".format("Process time:", ((System.nanoTime - t0)/1000000000.0)))

t0 = System.nanoTime

println("Cauchy")
val cauchy = new Cauchy(0.0, 0.5, 93897412816L)
val caurand = () => cauchy.sample
paths.foreach(p => {
  val price = MonteCarlo_BS.simpleVanilla(spot, ratedom, ratefor, sigma, NormSInv, caurand, flow, time, discount, p)
  println(p + " paths => " + price._1 + " stddev " + price._2)
})
println("%-27.27s %.3f sec".format("Process time:", ((System.nanoTime - t0)/1000000000.0)))
