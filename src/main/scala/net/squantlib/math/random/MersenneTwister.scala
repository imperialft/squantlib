package net.squantlib.math.random

import org.apache.commons.math3.random.{MersenneTwister => MT}

/**
 * Lazy random number Generator using MersenneTwister.
 * Actual generation is done by the implementation from Apache Commons Math3.
 *
 * @param seed A seed number for the sequence.
 */
class MersenneTwister(val seed:Long) extends RandomGenerator {
  
  override def toString = "MersenneTwister[Double]"
    
  val generator = new MT(seed)
  
  def reset = generator match { case m:MT => m.setSeed(seed)}
}
