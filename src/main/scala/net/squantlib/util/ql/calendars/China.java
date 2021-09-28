/*
 Copyright (C) 2008 Tim Swetonic, Jia Jia

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
import static net.squantlib.util.ql.Weekday.Saturday;
import static net.squantlib.util.ql.Weekday.Sunday;

/**
 * Chinese calendar Holidays:
 * <ul>
 * <li>Saturdays</li>
 * <li>SUNDAYs</li>
 * <li>New Year's day, JANUARY 1st (possibly followed by one or two more holidays)</li>
 * <li>Labour Day, first week in May</li>
 * <li>National Day, one week from October 1st</li>
 * </ul>
 *
 * Other holidays for which no rule is given (data available for 2004-2008 only):
 * <ul>
 * <li>Chinese New Year</li>
 * <li>Ching Ming Festival</li>
 * <li>Tuen Ng Festival</li>
 * <li>Mid-Autumn Festival</li>
 * </ul>
 *
 * Data from <http://www.sse.com.cn/sseportal/en_us/ps/home.shtml>
 *
 * \ingroup calendars
 *
 * @see <a href="http://www.sse.com.cn/sseportal/webapp/cm/keyWordSearchEn?KeyWord=holiday&page=1&x=0&y=0">SSE Holidays</a>
 *
 * @author Tim Swetonic
 * @author Jia Jia
 * @author Renjith Nair
 * @author Zahid Hussain
 */

@QualityAssurance(quality = Quality.Q3_DOCUMENTATION, version = Version.V097, reviewers = { "Zahid Hussain" })
public class China extends Calendar {

    public static enum Market {
        /**
         * Shanghai stock exchange
         */
        SSE
    }

    //
    // public constructors
    //

    public China() {
        this(Market.SSE);
    }

    public China(final Market m) {
        switch (m) {
        case SSE:
            impl = new SseImpl();
            break;
        default:
            throw new LibraryException(UNKNOWN_MARKET);
        }
    }

    //
    // private final inner classes
    //

    private final class SseImpl extends Impl {

        @Override
        public String name() {
            return "Shanghai stock exchange";
        }

        @Override
        public boolean isWeekend(final Weekday w) {
            return w == Saturday || w == Sunday;
        }

        @Override
        public boolean isBusinessDay(final Date date) {
            final Weekday w = date.weekday();
            final int d = date.dayOfMonth();
            final Month m = date.month();
            final int y = date.year();

            if (isWeekend(w)
                    // New Year's Day
                    || (d == 1 && m == January)
                    || (d == 3 && m == January && y == 2005)
                    || ((d == 2 || d == 3) && m == January && y == 2006)
                    || (d <= 3 && m == January && y == 2007)
                    || (d == 31 && m == December && y == 2007)
                    || (d == 1 && m == January && y == 2008)
                    || (d == 1 && m == January && y == 200)
                    || (d == 2 && m == January && y == 2009)
                    // Chinese New Year
                    || (d >= 19 && d <= 28 && m == January && y == 2004) || (d >= 7 && d <= 15 && m == February && y == 2005)
                    || (((d >= 26 && m == January) || (d <= 3 && m == February)) && y == 2006)
                    || (d >= 17 && d <= 25 && m == February && y == 2007)
                    || (d >= 6 && d <= 12 && m == February && y == 2008)
                    || (d >= 26 && d <= 30 && m == January && y == 2009)
                    // Ching Ming Festival
                    || (d == 4 && m == April && y == 2008)
                    || (d == 6 && m == April && y == 2009)
                    // Labor Day
                    || (d >= 1 && d <= 7 && m == May && y <= 2007) || (d >= 1 && d <= 2 && m == May && y == 2008)
                    || (d == 1 && m == May && y == 2009)
                    // Tuen Ng Festival
                    || (d == 9 && m == June && y == 2008) || (d >= 28 && d <= 29 && m == May && y == 2009)
                    // Mid-Autumn Festival
                    || (d == 15 && m == September && y == 2008)
                    // National Day
                    || (d >= 1 && d <= 7 && m == October && y <= 2007) || (d >= 29 && m == September && y == 2008)
                    || (d <= 3 && m == October && y == 2008) || (d >= 1 && d <= 8 && m == October && y == 2009)) {
                return false;
            }
            return true;

        }
    }
}
