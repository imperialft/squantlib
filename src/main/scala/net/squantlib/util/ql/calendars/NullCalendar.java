/*
 Copyright (C) 2008 Srinivas Hasti
 Copyright (C) 2008 Dominik Holenstein

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
import net.squantlib.util.ql.Calendar;
import net.squantlib.util.ql.Date;
import net.squantlib.util.ql.Weekday;

/**
 * Calendar for reproducing theoretical calculations
 *
 * @author Richard Gomes
 */
@QualityAssurance(quality = Quality.Q3_DOCUMENTATION, version = Version.V097, reviewers = { "Richard Gomes" })
public class NullCalendar extends Calendar {

    //
    // public constructors
    //

    public NullCalendar() {
        impl = new Impl();
    }


    //
    // private final inner classes
    //

    private final class Impl extends Calendar.Impl {
        @Override
        public String name() /* @ReadOnly */{
            return "Null";
        }

        @Override
        public boolean isWeekend(final Weekday weekday) /* @ReadOnly */{
            return false;
        }

        @Override
        public boolean isBusinessDay(final Date date) /* @ReadOnly */{
            return true;
        }
    }

}
