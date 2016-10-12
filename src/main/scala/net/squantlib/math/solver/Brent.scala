package net.squantlib.math.solver

import annotation.tailrec
import org.apache.commons.math3.analysis.solvers.BrentSolver
import org.apache.commons.math3.analysis.UnivariateFunction

object Brent extends RangedRootFinder {
  
   override def solve(f:Double => Double, 
       xmin:Double,
       xmax:Double,
       xAccuracy:Double = defaultAccuracy, 
       maxIteration:Int = defaultIteration
       ):Option[Double] = {
     
	   val func = new UnivariateFunction {def value(v:Double) = f(v)}
	   val solver = new BrentSolver(xAccuracy)
	   try {Some(solver.solve(maxIteration, func, xmin, xmax))} catch {case _:Throwable => None}
   }
}
