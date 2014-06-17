package net.squantlib.math.random

import scala.language.reflectiveCalls

/**
 * Abstract random number Generator class. Numbers are lazily generated to save memory.
 *
 */
trait RandomGenerator extends Stream[Double] {
  override def toString = "Generator[Double]"

  override def head() = sum_12_samples_then_minus_6
  override def tail = this

  /**
   * Set this Stream to be always filled.
   */
  override def isEmpty = false

  /**
   * Set this Stream to be infinitely long.
   */
  protected def tailDefined = false

  /**
   * An abstract accessor to random number generator implementation.
   *
   */
  protected val generator:{def nextDouble():Double}

  /**
   * An abstract function to reset the sequence for pseudo-random generators
   *
   */
  def reset
  
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
  protected def sum_12_samples_then_minus_6():Double = {
    var sum:Double = 0
    for (i <- 1 to 12)
      sum += sample()
    sum - 6
  }

}


