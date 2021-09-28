/*
 Copyright (C) 2012 Masakatsu Wakayu

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

import net.squantlib.util.ql.Calendar;
import net.squantlib.util.ql.Date;
import net.squantlib.util.ql.Month;
import net.squantlib.util.ql.Weekday;

import static net.squantlib.util.ql.Month.*;


/**
 *  Romanian calendar
 *  Holidays:
 *       <ul>
 *       <li>Saturdays</li>
 *       <li>Sundays</li>
 *       <li>Easter Monday</li>
 *       <li>Whit(Pentecost) Monday </li>
 *       <li>New Year's Day, January 1st, 2nd</li>
 *       <li>Labour Day, May 1st</li>
 *       <li>Dormition of the Theotokos, August 15th</li>
 *       <li>Bank Holiday, November 30th</li>
 *       <li>Great Union Day, December 1st</li>
 *       <li>Christmas, December 25th</li>
 *       <li>2nd Day of Christmas, December 26th</li>
 *       </ul>
 *       in group calendars
 *
 * @author Masakatsu Wakayu
 *
 */

//@QualityAssurance(quality = Quality.Q3_DOCUMENTATION, version = Version.V097, reviewers = { "" })

public class Romania extends Calendar {

    //
    // public constructors
    //

	public Romania() {
		impl = new Impl();
	}

    //
    // private final inner classes
    //

    private final class Impl extends WesternImpl {

    	@Override
    	public String name() { return "Hungary"; }

    	@Override
        public boolean isBusinessDay(final Date date) {
            final Weekday w = date.weekday();
            final int d = date.dayOfMonth(), dd = date.dayOfYear();
            final Month m = date.month();
            final int y = date.year();
            final int em = easterMonday(y);
            if (isWeekend(w)
                // Easter Monday
                || (dd == em)
                // Whit Monday
                || (dd == em+49)
                // New Year's Day
                || (d == 1  && m == January)
                || (d == 2  && m == January)
                // Labour Day
                || (d == 1  && m == May)
                // Dormition of the Theotokos
                || (d == 15  && m == August)
                // Bank Holiday
                || (d == 30  && m == November)
                // Great Union Day
                || (d == 1  && m == December)
                // Christmas
                || (d == 25 && m == December)
                // 2nd Day of Christmas
                || (d == 26 && m == December)) {
                return false;
            }
            return true;
        }
    }
}
