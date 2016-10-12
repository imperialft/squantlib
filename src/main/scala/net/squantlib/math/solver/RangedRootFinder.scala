package net.squantlib.math.solver

trait RangedRootFinder {
  
   var defaultAccuracy = 0.00001
   var defaultIteration = 20
  
   def solve(f:Double => Double, 
       xmin:Double,
       xmax:Double,
       xAccuracy:Double = defaultAccuracy, 
       maxIteration:Int = defaultIteration
       ):Option[Double]
   
}
