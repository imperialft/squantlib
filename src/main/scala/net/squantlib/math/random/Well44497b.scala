package net.squantlib.math.random

import org.apache.commons.math3.random.{Well44497b => Well}

class Well44497b(val seed:Long) extends RandomGenerator {
  val generator = new Well(seed)
  def reset = generator match { case gen:Well => gen.setSeed(seed)}
}
