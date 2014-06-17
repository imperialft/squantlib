package net.squantlib.math.statistical

import scala.language.postfixOps
import org.apache.commons.math3.linear.CholeskyDecomposition
import org.apache.commons.math3.linear.{ArrayRealVector, Array2DRowRealMatrix}
import scala.annotation.tailrec

object Cholesky {
  
  def decomposition(a:Array[Array[Double]]):Option[(Array[Array[Double]], Array[Double])] = {
    
    val acopy = a.map(_.clone)
    
    @tailrec def multSum(a:Array[Double], b:Array[Double], v:Double = 0):Double = if (a isEmpty) v else multSum(a.tail, b.tail, a.head * b.head + v)
  
    @tailrec def sqSum(a:Array[Double], v:Double = 0):Double = if (a isEmpty) v else sqSum(a.tail, a.head * a.head + v)
    
    val diag = Array.tabulate(a.size){i =>
      val aLine_i = acopy(i)
      val dsum = aLine_i(i) - sqSum(aLine_i.take(i))
      if (dsum <= 0.0) return None
      val diagg = math.sqrt(dsum)
      
      Range(i+1, acopy.size).map{j =>
        val summ = aLine_i(j) - multSum(aLine_i.take(i), acopy(j).take(i))
        acopy(j)(i) = summ / diagg
      }
      diagg
    }
    
    Some(acopy, diag)
  }
  
  def solver(a:Array[Array[Double]], p:Array[Double], b:Array[Double]):Array[Double] ={
    
	@tailrec def multSum(a:List[Double], b:List[Double], v:Double = 0):Double = if (a.isEmpty || b.isEmpty) v else multSum(a.tail, b.tail, a.head * b.head + v)
    
    @tailrec def recf(alist:List[List[Double]], blist:List[Double], plist:List[Double], x:List[Double]):List[Double] = alist match {
      case Nil => x.reverse
      case aLine_i :: rest => recf(rest, blist.tail, plist.tail, (blist.head - multSum(aLine_i, x.reverse)) / plist.head :: x)
    }
    
    val alist = a.toList.map(_.toList)
    val x1 = recf(alist, b.toList, p.toList, List.empty)
    recf(alist.transpose.map(_.reverse).reverse, x1.reverse, p.toList.reverse, List.empty).reverse.toArray
  }

  def getDecomposer(a:Array[Array[Double]]) = new CholeskyDecomposition(new Array2DRowRealMatrix(a))
  
  def decompositionWithLib(a:Array[Array[Double]]) = getDecomposer(a).getL.getData
  
  def solverWithLib(a:Array[Array[Double]], b:Array[Double]) = getDecomposer(a).getSolver.solve(new ArrayRealVector(b)).toArray
  
}


