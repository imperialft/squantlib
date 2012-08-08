package squantlib.math.random

import org.apache.commons.math3.random.{MersenneTwister => MT}
import java.util.Random

/**
 * Abstract random number Generator class. Numbers are lazily generated to save memory.
 *
 */
abstract class Generator extends Stream[Double] {
  override def toString = "RandomNumberGenerator[Double]"

  override def head() = sum_12_samples_then_minus_6
  override def tail = this

  override def isEmpty = false

  protected

  def tailDefined = false

  /**
   * Generates next random number.
   *
   * @return A random number.
   */
  def sample():Double

  /**
   * Generates a near-Gaussian random number.
   *
   * @return A random number in a near-Gaussian distribution.
   */
  def sum_12_samples_then_minus_6():Double = {
    var sum:Double = 0
    for (i <- 1 to 12)
      sum += sample()
    sum - 6
  }

}

/**
 * Lazy random number Generator using MersenneTwister.
 *
 * @param seed A seed number for the sequence.
 */
class MersenneTwister(seed:Long) extends Generator {
  val mt = new MT(seed)
  def sample() = mt.nextDouble()
}

/**
 * Lazy random number Generator using java.util.Random.
 *
 * @param seed A seed number for the sequence.
 */
class Java(seed:Long) extends Generator {
  val rand = new Random(seed)
  def sample() = rand.nextDouble()
}