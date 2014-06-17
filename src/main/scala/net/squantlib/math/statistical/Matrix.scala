package net.squantlib.math.statistical

import scala.annotation.tailrec

object Mathematical {
  
  def lowerTriangleMatrixMult(m:Array[Array[Double]], x:Array[Double]):Array[Double] = 
    Array.tabulate(m.size)(n => {var y = 0.0; for (j <- 0 to n) y += m(n)(j) * x(j); y})
    
  def lowerTriangleMatrixMult(m:Array[Array[Double]], x:List[Double]):List[Double] = {
    
    @tailrec def acc(xs:List[Double], vs:List[Double], n:Int):List[Double] = 
      if (xs.isEmpty) vs.reverse
      else acc(xs.tail, {var y = 0.0; for (j <- 0 to n) y += m(n)(j) * x(j); y} :: vs, n + 1)
    
    acc(x, List.empty, 0)
  }
  
  
  def CND(d:Double):Double = {
    val A1 = 0.31938153
    val A2 = -0.356563782
    val A3 = 1.781477937
    val A4 = -1.821255978
    val A5 = 1.330274429
    val RSQRT2PI = 0.39894228040143267793994605993438

    val k = 1.0 / (1.0 + 0.2316419 * math.abs(d))
    val cnd = RSQRT2PI * math.exp(-0.5 * d * d) * (k * (A1 + k * (A2 + k * (A3 + k * (A4 + k * A5)))))

    if (d > 0) 1.0 - cnd else cnd
  }

  def BivariateCND(a:Double, b:Double, rho:Double):Double = {
    val Aarray = Array(0.24840615, 0.39233107, 0.21141819, 0.03324666, 0.00082485334)
    val Barray = Array(0.10024215, 0.48281397, 1.0609498, 1.7797294, 2.6697604)
    
    val ap = a / math.sqrt(2 * (1 - rho * rho))
    val bp = b / math.sqrt(2 * (1 - rho * rho))
    
    def sign(x:Double):Double = if (x > 0) 1.0 else if(x < 0) -1.0 else 0.0

    a match {
      case _ if a <= 0 && b <= 0 && rho <= 0 =>
        var sum = 0.0
        for (i <- 0 to 4; j <- 0 to 4) {sum += Aarray(i) * Aarray(j) * IntermediaryFunctionBivariate(Barray(i), Barray(j), ap, bp, rho)}
        sum * math.sqrt(1 - rho * rho) / Math.PI
        
      case _ if a <= 0 && b >= 0 && rho >= 0 => CND(a) - BivariateCND(a, -b, -rho)

      case _ if a >= 0 && b <= 0 && rho >= 0 => CND(b) - BivariateCND(-a, b, -rho)
      
      case _ if a >= 0 && b >= 0 && rho <= 0 => CND(a) + CND(b) - 1 + BivariateCND(-a, -b, rho)
      
      case _ if a * b * rho >= 0 =>
        val denum = math.sqrt(a *a - 2 * rho * a * b + b *b)
        val rho1 = (rho * a - b) * sign(a) / denum
        val rho2 = (rho * b - a) * sign(b) / denum
        val delta = (1 - sign(a) * sign(b)) / 4
        BivariateCND(a, 0, rho1) + BivariateCND(b, 0, rho2) - delta
        
      case _ => -1
      }
  }

  private def IntermediaryFunctionBivariate(x:Double, y:Double, ap:Double, bp:Double, rho:Double):Double = {
    val r = ap * (2 * x - ap) + bp * (2 * y - bp) + 2 * rho * (x - ap) * (y - bp)
    math.exp(r)
  }
        
}