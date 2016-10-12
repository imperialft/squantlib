package net.squantlib.math.random

import scala.annotation.tailrec
import scala.collection.mutable.Queue

class ParkMiller(seed:Long) extends RandomGenerator {
  
  val IA:Long =16807
  val IM:Long =2147483647
  val AM:Double = 1.0 / IM
  val IQ:Long = 127773
  val IR:Long = 2836
  val NTAB:Int = 32 
  val NDIV:Double = 1 + (IM-1) / NTAB
  val EPS:Double = 1.2e-7
  val RNMX:Double = 1.0-EPS
   
  var initialized:Boolean = false
  
  override def toString = "ParkMiller[" + seed + "]"
  
  // used by ran1
  var iy:Long = 0
  
  // used by gasdev
  var hasItem = false
  var nextItem = 0.0
  def getItem = {hasItem = false; nextItem}
  def putItem(d:Double) = {hasItem = true; nextItem = d}

  // used by all
  var idum:Long = 1
  var iv:Array[Long] = Array.fill(NTAB)(0)
  
  def initialize:Unit = initialize(1)

  def initialize(initIdum:Long):Unit = {
    idum = math.max(initIdum, 1)
    hasItem = false
    initialized = false
  }
  
  override val generator = this

  def nextDouble():Double = uniform
  
  override def reset = initialize(seed)
  
  def uniform:Double = {
	// this is the ran1 function of Numerical recipes
    // random number generator of Park and Miller with Bays-Durham
	//safeguards. Returns a uniform random deviate between 0.0 and 1.0 (exclusive of the endpoint
	//values). Call with idum a negative integer to initialize; thereafter, do not alter idum between
	//successive deviates in a sequence. RNMX should approximate the largest floating value that is
	//less than 1.

    def shuffle = {
      val k = idum / IQ
      idum = IA * (idum - k * IQ) - IR * k //Compute idum=(IA*idum) % IM without over-
      if (idum < 0) idum += IM 
    }
    
    //Initialize if necessary 
    if (!initialized){
      
      initialized = true
      
      for (i <- 0 to 7) shuffle // 8 warm-ups
      
      iv = Array.fill(NTAB){shuffle; idum}.reverse // Load the shuffle table
        
      iy = iv(0)
    }
    
    // Start here when not initializing.
    shuffle
    
    val j = (iy / NDIV).toInt //Will be in the range 0..NTAB-1.
    
    iy = iv(j) //Output previously stored value and refill
    
    iv(j) = idum // shuffle table.
    
    (AM * iy) match {
      case t if t > RNMX => RNMX
      case t => t
    }
  }
  
  @tailrec private def gaussGenerate:(Double, Double, Double) = {
    val v1 = 2.0 * uniform - 1.0	// pick two uniform numbers in the square ex-
    val v2 = 2.0 * uniform - 1.0 	// tending from -1 to +1 in each direction,
    val rsq = v1 * v1 + v2 * v2			// see if they are in the unit circle,
    if (rsq >= 1.0 || rsq == 0.0) gaussGenerate else (v1, v2, rsq) // and if they are not, try again.
  }
  
  def gaussian:Double = {
    // this is the gasdev function of numerical recipes
    // Returns a normally distributed deviate with zero mean and unit variance, using ran1(idum)
    // as the source of uniform deviates.
    
    if (hasItem) getItem
    else {
      val (v1, v2, rsq) = gaussGenerate
      val fac = math.sqrt(-2.0 * math.log(rsq) / rsq)
      putItem(v1 * fac) //Box-Muller transformation to get extra normal deviates
      v2 * fac
    }
  }
  
  
  def gaussians(n:Int):scala.collection.mutable.Queue[Double] = {
    // this is the gasdev function of numerical recipes
    // Returns a normally distributed deviate with zero mean and unit variance, using ran1(idum)
    // as the source of uniform deviates.
    
    
    val q = new Queue[Double]
    var currentsize = 0
    
    if (hasItem) {
      q += getItem
      currentsize += 1
    }
    
    while(currentsize < n) {
      val (v1, v2, rsq) = gaussGenerate
      val fac = math.sqrt(-2.0 * math.log(rsq) / rsq)
      q += v2 * fac
      currentsize += 1
      if (currentsize < n) {q += v1 * fac; currentsize += 1}
      else putItem(v1 * fac) //Box-Muller transformation to get extra normal deviates
    }
    q
  }  
}

