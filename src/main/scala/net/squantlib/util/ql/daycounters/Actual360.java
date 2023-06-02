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

/**
 * Actual/360 day count convention, also known as "Act/360", or "A/360".
 *
 * @author Richard Gomes
 * @author Srinivas Hasti
 * @category daycounters
 * @see <a href="http://en.wikipedia.org/wiki/Day_count_convention">Day count Convention</a>
 */
@QualityAssurance(quality = Quality.Q4_UNIT, version = Version.V097, reviewers = "Richard Gomes")
public class Actual360 extends DayCounter {


  public Actual360() {
    super.impl = new Impl();
  }

  //
  // annual daycount (est)
  //

  public double annualDayCount() {
    return (365.0 / 360.0);
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
      return "Actual/360";
    }

    @Override
    public /*@Time*/ final double yearFraction(
      final Date dateStart,
      final Date dateEnd,
      final Date refPeriodStart,
      final Date refPeriodEnd,
      final Boolean isTerminationDate
    ) /* @ReadOnly */ {
      return /*@Time*/ dayCount(dateStart, dateEnd) / 360.0;
    }

  }


}
