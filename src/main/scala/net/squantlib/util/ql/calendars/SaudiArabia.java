/*
 Copyright (C) 2008

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

package net.squantlib.util.ql.calendars;

import net.squantlib.util.ql.lang.annotation.QualityAssurance;
import net.squantlib.util.ql.lang.annotation.QualityAssurance.Quality;
import net.squantlib.util.ql.lang.annotation.QualityAssurance.Version;
import net.squantlib.util.ql.lang.exceptions.LibraryException;
import net.squantlib.util.ql.Calendar;
import net.squantlib.util.ql.Date;
import net.squantlib.util.ql.Month;
import net.squantlib.util.ql.Weekday;

import static net.squantlib.util.ql.Month.*;
import static net.squantlib.util.ql.Weekday.Friday;
import static net.squantlib.util.ql.Weekday.Thursday;

/**
 * Saudi Arabia calendar
 * <ul>
 * <li>Thursdays</li>
 * <li>Fridays</li>
 * <li>National Day of Saudi Arabia, September 23rd</li>
 * <p>
 * Other holidays for which no rule is given (data available for 2004-2005 only:)
 * </p>
 * <li>Eid Al-Adha</li>
 * <li>Eid Al-Fitr</li>
 * </ul>
 *
 * @author Richard Gomes
 */

@QualityAssurance(quality = Quality.Q3_DOCUMENTATION, version = Version.V097, reviewers = { "Zahid Hussain" })

public class SaudiArabia extends Calendar{

    public static enum Market {
        /**
         * Tadawul financial market
         */
        Tadawul
    }


    //
    // public constructors
    //

    public SaudiArabia() {
	   this(Market.Tadawul);
    }

    public SaudiArabia(final Market m) {
        switch (m) {
          case Tadawul:
            impl = new TadawulImpl();
            break;
          default:
              throw new LibraryException(UNKNOWN_MARKET);
        }
    }


    //
    // private final inner classes
    //

    private final class TadawulImpl extends Impl {
		@Override
		public String name() { return "Tadawul"; }

		@Override
		public boolean isWeekend(final Weekday w) {
		   return w == Thursday || w == Friday;
		}

		@Override
		public boolean isBusinessDay(final Date date)  {
			final Weekday w = date.weekday();
			final int d = date.dayOfMonth();
			final Month m = date.month();
			final int y = date.year();
			if (isWeekend(w)
					// National Day
					|| (d == 23 && m == September)
					// Eid Al-Adha
					|| (d >= 1 && d <= 6 && m == February && y == 2004)
					|| (d >= 21 && d <= 25 && m == January && y == 2005)
					// Eid Al-Fitr
					|| (d >= 25 && d <= 29 && m == November && y == 2004)
					|| (d >= 14 && d <= 18 && m == November && y == 2005)) {
                return false;
            }
			return true;
		}
    }
 }

