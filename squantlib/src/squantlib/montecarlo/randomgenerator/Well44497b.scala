package squantlib.montecarlo.randomgenerator

class Well44497b(seed:Long) extends RandomGenerator {
  val generator = new org.apache.commons.math3.random.Well44497b(seed)
}
