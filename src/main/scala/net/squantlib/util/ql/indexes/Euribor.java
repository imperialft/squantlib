/*
 Copyright (C) 2008 Srinivas Hasti

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
package net.squantlib.util.ql.indexes;

import net.squantlib.util.ql.QL;
import net.squantlib.util.ql.currencies.Europe.EURCurrency;
import net.squantlib.util.ql.daycounters.Actual360;
import net.squantlib.util.ql.termstructures.YieldTermStructure;
import net.squantlib.util.ql.time.Period;
import net.squantlib.util.ql.time.TimeUnit;
import net.squantlib.util.ql.time.calendars.Target;

/**
 * Euribor index
 * <p>
 * Euribor rate fixed by the ECB.
 *
 * @author Srinivas Hasti
 * @note This is the rate fixed by the ECB. Use EurLibor if you're interested in the London fixing by BBA.
 */
public class Euribor extends IborIndex {

  public Euribor(final Period tenor) {
    this(tenor, null);
  }

  public Euribor(
    final Period tenor,
    final YieldTermStructure h
  ) {
    super("Euribor",
      tenor,
      2, // settlement days
      new EURCurrency(),
      new Target(),
      euriborConvention(tenor),
      euriborEOM(tenor),
      new Actual360(),
      h);
    QL.require(tenor().units() != TimeUnit.Days, "for daily tenors dedicated DailyTenor constructor must be used");
  }

}
