package squantlib.montecarlo.randomgenerator

/**
 * Lazy random number Generator using java.util.Random.
 *
 * @param seed A seed number for the sequence.
 */
class Java(seed:Long) extends RandomGenerator {
  override def toString = "java.util.Random[Double]"
  val generator = new java.util.Random(seed)
}
