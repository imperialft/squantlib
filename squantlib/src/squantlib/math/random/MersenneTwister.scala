package squantlib.math.random

import org.apache.commons.math3.random.{MersenneTwister => MT}

/**
 * Lazy random number Generator using MersenneTwister.
 * Actual generation is done by the implementation from Apache Commons Math3.
 *
 * @param seed A seed number for the sequence.
 */
class MersenneTwister(seed:Long) extends Generator {
  override def toString = "MersenneTwister[Double]"
  val generator = new MT(seed)
}
