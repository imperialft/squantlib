package net.squantlib.math.random

/**
 * Lazy random number Generator using java.util.Random.
 *
 * @param seed A seed number for the sequence.
 */
class Java(val seed:Long) extends RandomGenerator {
  override def toString = "java.util.Random[Double]"
  val generator = new java.util.Random(seed)
  def reset = generator match { case gen:java.util.Random => gen.setSeed(seed)}
}
