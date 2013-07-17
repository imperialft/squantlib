
/*
Copyright (C) 2008 Richard Gomes

This source code is release under the BSD License.

This file is part of JQuantLib, a free-software/open-source library
for financial quantitative analysts and developers - http://jquantlib.org/

JQuantLib is free software: you can redistribute it and/or modify it
under the terms of the JQuantLib license.  You should have received a
copy of the license along with this program; if not, please email
<jquant-devel@lists.sourceforge.net>. The license is also available online at
<http://www.jquantlib.org/index.php/LICENSE.TXT>.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the license for more details.

JQuantLib is based on QuantLib. http://quantlib.org/
When applicable, the original copyright notice follows this notice.
*/

package org.jquantlib.math.solvers1D;

import org.jquantlib.math.AbstractSolver1D;
import org.jquantlib.math.Ops;

/**
* Incremental 1-D solver<br/>
* <p>
* Simple Newton-Raphson method.
* 
* @author Masakatsu Wakayu
*/
public class NewtonIncremental extends AbstractSolver1D<Ops.DoubleOp>  {

   /**
    * Computes the roots of a function by using the Bisection method.
    * @param f the function
    * @param xAccuracy the provided accuracy
    * @returns <code>root_</code>
    */
	
	double defaultfirststep = 0.01;
	
   @Override
   protected double solveImpl(final Ops.DoubleOp f, final double xAccuracy) {
       double shift = -defaultfirststep;
       double F = f.op(root);
       double Xprev, Fprev, dF;

       while (evaluationNumber <= getMaxEvaluations())
       {
           Xprev = root;
           Fprev = F;
           root = Xprev - shift;
           F = f.op(root);
           dF = (F - Fprev) / (root - Xprev);
           shift = F / dF;
           evaluationNumber++;
           if (Math.abs(F) < xAccuracy || F == 0.0)
               return root;
       }
       
        throw new ArithmeticException("maximum number of function evaluations exceeded"); // TODO: message
   }
}
