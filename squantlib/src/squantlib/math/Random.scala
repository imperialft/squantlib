package squantlib.math.random

import org.apache.commons.math3.random.{MersenneTwister => MT}

/**
 * Abstract random number Generator class. Numbers are lazily generated to save memory.
 *
 */
abstract class Generator extends Stream[Double] {
  override def toString = "Generator[Double]"

  override def head() = sum_12_samples_then_minus_6
  override def tail = this

  /**
   * Set this Stream to be always filled.
   */
  override def isEmpty = false

  protected

  /**
   * Set this Stream to be infinitely long.
   */
  def tailDefined = false

  /**
   * An abstract accessor to random number generator implementation.
   *
   */
  val generator:{def nextDouble():Double}

  /**
   * Generates next random number.
   *
   * @return A random number.
   */
  def sample():Double = generator.nextDouble();

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
 * Actual generation is done by the implementation from Apache Commons Math3.
 *
 * @param seed A seed number for the sequence.
 */
class MersenneTwister(seed:Long) extends Generator {
  override def toString = "MersenneTwister[Double]"
  val generator = new MT(seed)
}

/**
 * Lazy random number Generator using java.util.Random.
 *
 * @param seed A seed number for the sequence.
 */
class Java(seed:Long) extends Generator {
  override def toString = "java.util.Random[Double]"
  val generator = new java.util.Random(seed)
}                