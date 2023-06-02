/*
 Copyright (C) 2007 Richard Gomes

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
 Copyright (C) 2004 Ferdinando Ametrano
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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

package net.squantlib.util.ql.daycounters;

import net.squantlib.util.ql.lang.annotation.QualityAssurance;
import net.squantlib.util.ql.lang.annotation.QualityAssurance.Quality;
import net.squantlib.util.ql.lang.annotation.QualityAssurance.Version;
import net.squantlib.util.ql.time.Date;
import net.squantlib.util.ql.time.Period;
import net.squantlib.util.ql.time.TimeUnit;

/**
 * "Actual/365 (Fixed)" day count convention, also know as
 * "Act/365 (Fixed)", "A/365 (Fixed)", or "A/365F".
 *
 * @author Srinivas Hasti
 * @author Richard Gomes
 * @note According to ISDA, "Actual/365" (without "Fixed") is
 * an alias for "Actual/Actual (ISDA)"DayCounter (see
 * ActualActual.)  If Actual/365 is not explicitly
 * specified as fixed in an instrument specification,
 * you might want to double-check its meaning.
 */
@QualityAssurance(quality = Quality.Q4_UNIT, version = Version.V097, reviewers = "Richard Gomes")
public class Actual365Fixed extends DayCounter {

  public enum Convention {
    Fixed, Adjusted
  }


  //
  // public constructors
  //

  public Actual365Fixed() {
    this(Convention.Fixed, null);
  }

  public Actual365Fixed(
    final Actual365Fixed.Convention c,
    Period paymentPeriod
  ) {
    switch (c) {
      case Adjusted:
        super.impl = new ImplAdjusted(paymentPeriod);
        break;
      default:
        super.impl = new Impl();
        break;
    }
  }


//    public Actual365Fixed() {
//        super.impl = new Impl();
//    }


  //
  // annual daycount (est)
  //

  public double annualDayCount() {
    return 1.00;
  }

  //
  // private inner classes
  //

  final private class Impl extends DayCounter.Impl {

    //
    // implements DayCounter
    //

    @Override
    public final String name() /* @ReadOnly */ {
      return "Actual/365 (fixed)";
    }

    @Override
    public /*@Time*/ final double yearFraction(
      final Date dateStart,
      final Date dateEnd,
      final Date refPeriodStart,
      final Date refPeriodEnd,
      final Boolean isTerminationDate
    ) /* @ReadOnly */ {
      return /*@Time*/ dayCount(dateStart, dateEnd) / 365.0;
    }
  }

  final private class ImplAdjusted extends DayCounter.Impl {

    public Period paymentPeriod;
    public int months;

    public ImplAdjusted(Period p) {
      paymentPeriod = p;
      months = (paymentPeriod.units() == TimeUnit.Years) ? paymentPeriod.length() * 12 : paymentPeriod.length();
    }

    //
    // implements DayCounter
    //

    @Override
//        public final String name() /* @ReadOnly */{
//            return "Actual/365 (adjusted)";
//        }
    public final String name() /* @ReadOnly */ {
      return "Actual/365 (Adj " + months + "M)";
    }

    @Override
    public /*@Time*/ final double yearFraction(
      final Date dateStart,
      final Date dateEnd,
      final Date refPeriodStart,
      final Date refPeriodEnd,
      final Boolean isTerminationDate
    ) /* @ReadOnly */ {

      Date testEnd = dateStart.add(paymentPeriod);
      if (testEnd.eq(dateEnd) || (Date.isEndOfMonth(dateStart) && Date.isEndOfMonth(dateEnd) && Math.abs(testEnd.sub(dateEnd)) < 4)) {
        return months / 12.0;
      } else {
        return /*@Time*/ dayCount(dateStart, dateEnd) / 365.0;
      }
    }
  }

}
