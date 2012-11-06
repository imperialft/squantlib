package squantlib.math.random

/**
 * Lazy random number Generator using java.util.Random.
 *
 * @param seed A seed number for the sequence.
 */
class Java(seed:Long) extends Generator {
  override def toString = "java.util.Random[Double]"
  val generator = new java.util.Random(seed)
}
