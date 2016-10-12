package net.squantlib.math.solver

import annotation.tailrec

object Illinois extends RangedRootFinder {
  
   override def solve(f:Double => Double, 
       xmin:Double,
       xmax:Double,
       xAccuracy:Double = defaultAccuracy, 
       maxIteration:Int = defaultIteration
       ):Option[Double] = {
     
       @tailrec def solverAcc(vmin:Double, fmin:Double, vmax:Double, fmax:Double, iter:Int):Option[Double] = {
         val vnext = (0.5 * vmin * fmax - fmin * vmax) / (0.5 * fmax - fmin)
         if (fmin * fmax > 0) None
         else if (iter == 0) None
         else f(vnext) match {
           case fnext if math.abs(fnext) <= xAccuracy => Some(vnext)
           case fnext if fnext * fmin < 0.0 => solverAcc(vmin, fmin, vnext, fnext, iter-1)
           case fnext => solverAcc(vnext, fnext, vmax, fmax, iter-1)
         }}
       
       solverAcc(xmin, f(xmin), xmax, f(xmax), maxIteration)
   }
}
