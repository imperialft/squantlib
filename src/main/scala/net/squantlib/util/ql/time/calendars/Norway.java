/*
 Copyright (C) 2008 Anand Mani

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

package net.squantlib.util.ql.time.calendars;

import net.squantlib.util.ql.time.Calendar;
import net.squantlib.util.ql.time.Date;
import net.squantlib.util.ql.time.Month;
import net.squantlib.util.ql.time.Weekday;
import net.squantlib.util.ql.lang.annotation.QualityAssurance;
import net.squantlib.util.ql.lang.annotation.QualityAssurance.Quality;
import net.squantlib.util.ql.lang.annotation.QualityAssurance.Version;

import static net.squantlib.util.ql.time.Month.*;

/**
 * Norwegian calendar
 * <p>
 * Holidays:
 * <ul>
 * <li>Saturdays</li>
 * <li>Sundays</li>
 * <li>Holy Thursday</li>
 * <li>Good Friday</li>
 * <li>Easter Monday</li>
 * <li>Ascension</li>
 * <li>Whit(Pentecost) Monday</li>
 * <li>New Year's Day, JANUARY 1st</li>
 * <li>May Day, May 1st</li>
 * <li>National Independence Day, May 17st</li>
 * <li>Christmas, December 25th</li>
 * <li>Boxing Day, December 26th</li>
 * </ul>
 *
 * @category calendars
 *
 * @author Anand Mani
 * @author Zahid Hussain
 */

@QualityAssurance(quality = Quality.Q3_DOCUMENTATION, version = Version.V097, reviewers = { "Zahid Hussain" })

public class Norway extends Calendar {

    //
    // public constructors
    //

    public Norway() {
        impl = new Impl();
    }


    //
    // private final inner classes
    //

    private final class Impl extends WesternImpl {

		@Override
		public String name() { return "Norway"; }

		@Override
		public boolean isBusinessDay(final Date date) {
	        final Weekday w = date.weekday();
	        final int d = date.dayOfMonth(), dd = date.dayOfYear();
	        final Month m = date.month();
	        final int y = date.year();
	        final int em = easterMonday(y);
	        if (isWeekend(w)
	            // Holy Thursday
	            || (dd == em-4)
	            // Good Friday
	            || (dd == em-3)
	            // Easter Monday
	            || (dd == em)
	            // Ascension Thursday
	            || (dd == em+38)
	            // Whit Monday
	            || (dd == em+49)
	            // New Year's Day
	            || (d == 1  && m == January)
	            // May Day
	            || (d == 1  && m == May)
	            // National Independence Day
	            || (d == 17  && m == May)
	            // Christmas
	            || (d == 25 && m == December)
	            // Boxing Day
				|| (d == 26 && m == December)) {
                return false;
            }
	        return true;
	    }
    }
}
