/*
 Copyright (C) 2011 Tim Blackler

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
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006 StatPro Italia srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

package net.squantlib.util.ql.indexes.ibor;

import net.squantlib.util.ql.currencies.Oceania.AUDCurrency;
import net.squantlib.util.ql.daycounters.Actual360;
import net.squantlib.util.ql.termstructures.AbstractYieldTermStructure;
import net.squantlib.util.ql.termstructures.YieldTermStructure;
import net.squantlib.util.ql.time.Date;
import net.squantlib.util.ql.time.Period;
import net.squantlib.util.ql.time.calendars.Australia;

/**
 * Australian Dollar LIBOR fixed by BBA
 *
 * @see <a href="http://www.bba.org.uk/bba/jsp/polopoly.jsp?d=225&a=1414">http://www.bba.org.uk/bba/jsp/polopoly.jsp?d=225&a=1414</a>
 */
public class AUDLibor extends Libor {

  public AUDLibor(final Period tenor) {
    this(tenor,
      new AbstractYieldTermStructure() {
        @Override
        protected double discountImpl(final double t) {
          throw new UnsupportedOperationException();
        }

        @Override
        public Date maxDate() {
          throw new UnsupportedOperationException();
        }
      }
    );
  }

  public AUDLibor(
    final Period tenor,
    final YieldTermStructure h
  ) {
    super("AUDLibor", tenor, 2,
      new AUDCurrency(),
      new Australia(),
      new Actual360(), h);
  }

}
