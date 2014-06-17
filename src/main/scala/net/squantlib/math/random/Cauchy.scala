package net.squantlib.math.random

import java.lang.Math.{PI, tan, atan}

/**
 * Lazy random number generator with Cauchy-distribution (CDF) using MersenneTwister.
 *
 * @param location
 * @param scale
 * @param seed A seed number for the sequence.
 */

class Cauchy(val location:Double, val scale:Double, override val seed:Long) extends MersenneTwister(seed) {
  
  override def toString = "Cauchy[Double]"
    
  override def head = sample
  
  override def sample = {
    var n = generator.nextDouble
    while (n == 0.0) n = generator.nextDouble() // Cauchy needs n <= (0,1) instead of [0,1)
    // (location + scale * tan(PI * (n - 0.5)))
    val x = (location + scale * tan(PI * (n - 0.5)))
    1 / PI * atan((x - location) / scale) + 0.5 // return CDF(n)
  }
}