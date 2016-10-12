package net.squantlib.math.statistical

import org.apache.commons.math3.distribution.NormalDistribution

object NormSInv {
  
	def apply(u:Double):Double = moro(u)
  
	def apache(u:Double):Double = (new NormalDistribution).inverseCumulativeProbability(u)
	
	def moro(u:Double):Double ={
	  val (a0, a1, a2, a3) = (2.50662823884, -18.61500062529, 41.39119773534, -25.44106049637)
	  val (b1, b2, b3, b4) = (-8.47351093090, 23.08336743743, -21.06224101826, 3.13082909833)
	  val (c0, c1, c2) = (0.3374754822726147, 0.9761690190917186, 0.1607979714918209)
	  val (c3, c4, c5) = (0.0276438810333863, 0.0038405729373609, 0.0003951896511919)
	  val (c6, c7, c8) = (0.0000321767881768, 0.0000002888167364, 0.0000003960315187)
	
	  val v = u - 0.5
		
	  if (v > -0.42 && v < 0.42){
		val vv = v * v
		v * (((a3 * vv + a2) * vv + a1) * vv + a0)/((((b4 * vv + b3) * vv + b2) * vv + b1) * vv + 1.0)
	  }
	  else {
		val w = if (v > 0) math.log(-math.log(1-u)) else math.log(-math.log(u));
		val r=(((((((c8*w+c7)*w+c6)*w+c5)*w+c4)*w+c3)*w+c2)*w+c1)*w+c0;
		if (v < 0) -r else r
	  }
	}
	
	def acklam(p:Double):Double = {
      val a1:Double = -39.6968302866538       
      val a2:Double = 220.946098424521        
      val a3:Double = -275.928510446969       
      val a4:Double = 138.357751867269        
      val a5:Double = -30.6647980661472       
      val a6:Double = 2.50662827745924        
      
      val b1:Double = -54.4760987982241      
      val b2:Double = 161.585836858041        
      val b3:Double = -155.698979859887      
      val b4:Double = 66.8013118877197        
      val b5:Double = -13.2806815528857   
      
      val c1:Double = -7.78489400243029E-03
      val c2:Double = -0.322396458041136      
      val c3:Double = -2.40075827716184       
      val c4:Double = -2.54973253934373      
      val c5:Double = 4.37466414146497   
      val c6:Double = 2.93816398269878   
      
      val d1:Double = 7.78469570904146E-03 
      val d2:Double = 0.32246712907004      
      val d3:Double = 2.445134137143   
      val d4:Double = 3.75440866190742  
      
      val pl:Double = 0.02425 // lower bound
      val pu:Double = 1 - pl  // upper bound
      
      if (0 < p && p < pl) { // Rational approximation for lower region
            val q = math.sqrt(-2.0 * math.log(p))
            (((((c1 * q + c2) * q + c3) * q + c4) * q + c5) * q + c6) / ((((d1 * q + d2) * q + d3) * q + d4) * q + 1.0)
      }
      else if (pl <= p && p <= pu){ // Rational approximation for central region
            val q = p - 0.5
            val r = q * q
            (((((a1 * r + a2) * r + a3) * r + a4) * r + a5) * r + a6) * q / (((((b1 * r + b2) * r + b3) * r + b4) * r + b5) * r + 1.0)
      }
      else if (pu < p && p < 1) {// Rational approximation for upper region
        val q = math.sqrt(-2.0 * math.log(1.0 - p))
        -(((((c1 * q + c2) * q + c3) * q + c4) * q + c5) * q + c6) / ((((d1 * q + d2) * q + d3) * q + d4) * q + 1.0)
      }
      else math.log(-1.0)
	}
	
	
}