/*
 Copyright (C) 2008 Renjith Nair

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
 Copyright (C) 2005, 2007 StatPro Italia srl

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.
 */

package org.jquantlib.time.calendars;

import static org.jquantlib.time.Month.January;
import static org.jquantlib.time.Month.June;
import static org.jquantlib.time.Month.February;
import static org.jquantlib.time.Month.March;
import static org.jquantlib.time.Month.November;
import static org.jquantlib.time.Month.May;
import static org.jquantlib.time.Weekday.Monday;
import static org.jquantlib.time.Weekday.Tuesday;

import org.jquantlib.lang.annotation.QualityAssurance;
import org.jquantlib.lang.annotation.QualityAssurance.Quality;
import org.jquantlib.lang.annotation.QualityAssurance.Version;
import org.jquantlib.time.Calendar;
import org.jquantlib.time.Date;
import org.jquantlib.time.Month;
import org.jquantlib.time.Weekday;

/**
 * ! Holidays for the Russian stock exchange 
 * <ul>
 * <li>Saturdays</li>
 * <li>Sundays</li>
 * <li>New Year's Day, JANUARY 1st & 2nd</li>
 * <li>Orthodox Christmas, JANUARY 7th</li>
 * <li>Defernder of the Father's Land Day, February 23rd</li>
 * <li>International Women's Day, March 8th</li>
 * <li>International Workers Solidarity Days, May 1st and 2nd</li>
 * <li>Victory Day, May 9th</li>
 * <li>Russia Day, June 12th</li>
 * <li>National Unity Day, November 3rd, 4th</li>
 * </ul>
 * Holidays falling on a Saturday or Sunday are moved to the following Monday.
 *
 * @author Masakatsu Wakayu
 */
 
@QualityAssurance(quality = Quality.Q3_DOCUMENTATION, version = Version.V097, reviewers = { "None" })
public class Russia extends Calendar {
    public static enum Market {
        /**
         * Russian Stock Exchange
         */
        USE
    }

    //
    // public constructors
    //

    public Russia() {
        this(Market.USE);
    }

    public Russia(final Market m) {
        impl = new UseImpl();
    }

    //
    // private final inner classes
    //

    private final class UseImpl extends OrthodoxImpl {

        @Override
        public String name() {
            return "Ukrainian stock exchange";
        }

        @Override
        public boolean isBusinessDay(final Date date) {
            final Weekday w = date.weekday();
            final int d = date.dayOfMonth(), dd = date.dayOfYear();
            final Month m = date.month();
            final int y = date.year();
//            final int em = easterMonday(y);
            if (isWeekend(w) 
            // New Year's Day
                    || (d == 1 && m == January)
                    || (d == 2 && m == January)
                    // Orthodox Christmas
                    || ((d == 7 || ((d == 8 || d == 9) && w == Monday)) && m == January)
                    // Defender of the Fatherland Day
                    || ((d == 23 || ((d == 24 || d == 25) && w == Monday)) && m == February)
                    // Women's Day
                    || ((d == 8 || ((d == 9 || d == 10) && w == Monday)) && m == March)
                    // Workers' Solidarity Days
                    || ((d == 1 || d == 2 || (d == 3 && w == Monday)) && m == May)
                    // Victory Day
                    || ((d == 9 || ((d == 10 || d == 11) && w == Monday)) && m == May)
                    // Russia Day
                    || ((d == 12 || ((d == 13 || d == 14) && w == Monday)) && m == June)
                    // National Unity Day
                    || ((d == 3 || ((d == 4 || d == 5) && w == Monday)) && m == November)
                    || ((d == 4 || ((d == 5 || d == 6) && (w == Monday || w == Tuesday))) && m == November)) 
            {
                return false;
            }
            return true;
        }
    }
}