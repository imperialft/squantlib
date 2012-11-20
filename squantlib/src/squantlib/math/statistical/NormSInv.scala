package squantlib.math.statistical

object NormSInv {
	def apply(u:Double):Double =
	{
		// This function generates a standard normal random 
		// variable r from a uniform random variable in (0,1).
		// Note that u=0 or u=1 are not allowed.
	
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
}