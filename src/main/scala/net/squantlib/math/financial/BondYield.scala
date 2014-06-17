package net.squantlib.math.financial

import annotation.tailrec
import net.squantlib.math.solver.NewtonRaphson


object BondYield {
  
    def solve(price:Double, cashflows:List[(Double, Double)], yieldToZC:(Double, Double) => Double, accuracy:Double, maxIteration:Int):Option[Double] = {
      if (cashflows.exists(v => v._2.isNaN || v._2.isInfinity)) return None
      
      def yieldToPrice(y:Double):Double = cashflows.map{case (d, v) => v * yieldToZC(y, d)}.sum
      val priceformula = (y:Double) => (yieldToPrice(y) - price)
	  NewtonRaphson.solve(priceformula, 0.03, accuracy, 0.01, maxIteration)
	}

    def solveNoCompounding(price:Double, cashflows:List[(Double, Double)], accuracy:Double, maxIteration:Int):Option[Double] = 
      solve(price, cashflows, (y:Double, d:Double) => 1.0 / (1.0 + y * d), accuracy, maxIteration)
      
    def solveCompounded(price:Double, cashflows:List[(Double, Double)], freq:Int, accuracy:Double, maxIteration:Int):Option[Double] = {
      val f = freq.toDouble
      solve(price, cashflows, (y:Double, d:Double) => 1.0 / math.pow(1.0 + y / f, f * d), accuracy, maxIteration)
    }
      
    def solveContinuous(price:Double, cashflows:List[(Double, Double)], accuracy:Double, maxIteration:Int):Option[Double] = 
      solve(price, cashflows, (y:Double, d:Double) => math.exp(-y * d), accuracy, maxIteration)
      
    
    def solveNoRate(price:Double, cashflows:List[(Double, Double)], accrued:Double):Double = {
      (cashflows.unzip._2.sum - price) / (cashflows.unzip._1.max * (price - accrued))
    }
    
    def asAverageCoupon(cashflows:List[(Double, Double)], accrued:Double):Double = 
      (cashflows.unzip._2.sum - accrued - 1.0) / cashflows.unzip._1.max
      
}

