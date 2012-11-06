package squantlib.math.random

import scala.annotation.tailrec
import scala.collection.mutable.SynchronizedQueue

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
