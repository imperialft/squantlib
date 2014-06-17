package net.squantlib.math.solver

import annotation.tailrec

object NewtonRaphson {
  
   var defaultAccuracy = 0.00001
   var defaultFirstStep = 0.005
   var defaultIteration = 20
   var defaultMinBoundary:Option[Double] = None
   var defaultMaxBoundary:Option[Double] = None
  
   def solve(f:Double => Double, 
       initialValue:Double, 
       xAccuracy:Double = defaultAccuracy, 
       firstStep:Double = defaultFirstStep, 
       maxIteration:Int = defaultIteration
       ):Option[Double] = {
     
       @tailrec def solverAcc(shift:Double, x:Double, fx:Double, iter:Int):Option[Double] = {
         if (iter == 0) {if (math.abs(f(x)) <= xAccuracy) Some(x) else None}
         else if (shift.isNaN || shift.isInfinity) None
         else f(x) match {
           case fxnew if math.abs(fxnew) <= xAccuracy => Some(x)
           case fxnew => 
             var sf = fxnew / (fxnew - fx) * (-shift)
             solverAcc(sf, x - sf, fxnew, iter-1)
         }}
       
       solverAcc(firstStep, initialValue, f(initialValue + firstStep), maxIteration)
   }
}


