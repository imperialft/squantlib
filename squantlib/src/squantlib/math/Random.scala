package squantlib.math.random

import org.apache.commons.math3.random.{MersenneTwister => MT}
import scala.annotation.tailrec
import scala.collection.mutable.SynchronizedQueue

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


@tailrec class CorputBase2(var N:Long) extends Generator {
//   Returns the equivalent first van der Corput sequence number
  val generator = this

  def nextDouble():Double = {
      N = N + 1
      nextDouble(N, 0.0, 0.5)
  }
  
  def nextDouble(n1:Long, c:Double, ib:Double):Double = 
    if (n1 <= 0) c
    else {
//	    val n2:Long = (n1 / 2).toLong
	    val n2:Long = n1 / 2
		val i:Long = n1 - n2 * 2
		nextDouble(n2, c + ib * i, ib / 2.0)
    }
  
}

class CorputBase2_NR(var N:Long) extends Generator {
//   Returns the equivalent first van der Corput sequence number
  val generator = this

  def nextDouble():Double = {
    var n1:Long = N
    var c:Double = 0.0
    var ib:Double = 0.5
    while (n1 > 0){
	    val n2:Long = n1 / 2
		val i:Long = n1 - n2 * 2
		c = c + ib * i
		ib = ib / 2.0
		n1 = n2
    }
	N = N + 1
    c
  }
}


@tailrec class CorputBaseb(val b:Long, var N:Long) extends Generator {
//   Returns the equivalent first van der Corput sequence number
  val generator = this

  def nextDouble():Double = {
      N = N + 1
      nextDouble(N, 0.0, 1/b.toDouble)
  }
  
  def nextDouble(n1:Long, c:Double, ib:Double):Double = 
    if (n1 > 0) {
	    val n2:Long = (n1 / b).toLong
	    val i:Long = n1 - n2 * b
		nextDouble(n2, c + ib * i, ib / b)
    }
    else c
  
}

object CorputBase2 {
//   Returns the equivalent first van der Corput sequence number

  def generate(N:Long):Double = {
    var n1:Long = N
    var c:Double = 0.0
    var ib:Double = 0.5
    while (n1 > 0){
	    val n2:Long = n1 / 2
		val i:Long = n1 - n2 * 2
		c = c + ib * i
		ib = ib / 2.0
		n1 = n2
    }
    c
  }
  
  def generateSet(nbData:Long, initialN:Long):SynchronizedQueue[Double] = {
    val queue = new SynchronizedQueue[Double]
    (initialN to (initialN + nbData - 1)).par.foreach(n => queue.enqueue(generate(n)))
    queue
  }
}
