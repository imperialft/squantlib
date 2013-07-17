/*
 Copyright (C) 2009 Ueli Hofstetter

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
 Copyright (C) 2003, 2004 StatPro Italia srl

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

package org.jquantlib.termstructures.yieldcurves;

import org.jquantlib.QL;
import org.jquantlib.daycounters.DayCounter;

import org.jquantlib.quotes.Quote;
import org.jquantlib.termstructures.Compounding;
import org.jquantlib.termstructures.YieldTermStructure;
import org.jquantlib.time.Calendar;
import org.jquantlib.time.Date;
import org.jquantlib.time.Frequency;

/**
 * Term structure with added spread on the instantaneous forward rate
 *
 * @note This term structure will remain linked to the original structure, i.e., any changes in the latter will be reflected in this
 *       structure as well.
 *
 * @category yieldtermstructures
 *
 * @author Ueli Hofstetter
 */
public class ForwardSpreadedTermStructure extends ForwardRateStructure {

    //
    // private fields
    //

    private final YieldTermStructure originalCurve;
    private final Quote spread;


    //
    // public constructors
    //

    public ForwardSpreadedTermStructure(final YieldTermStructure h, final Quote spread) {
        QL.validateExperimentalMode();

        this.originalCurve = h;
        this.spread = spread;

        this.originalCurve.addObserver(this);
        this.spread.addObserver(this);
    }


    //
    // overrides TermStructure
    //

    @Override
    public DayCounter dayCounter() {
        return originalCurve.dayCounter();
    }

    @Override
    public Calendar calendar() {
        return originalCurve.calendar();
    }

    @Override
    public Date referenceDate() {
        return originalCurve.referenceDate();
    }

    @Override
    public Date maxDate() {
        return originalCurve.maxDate();
    }

    @Override
    public double maxTime() {
        return originalCurve.maxTime();
    }


    //
    // overrides ForwardRateStructure
    //

    @Override
    protected double forwardImpl(final double t) {
        return originalCurve.forwardRate(
                t, t, Compounding.Continuous, Frequency.NoFrequency, true).rate() + spread.value();
    }

    @Override
    public double zeroYieldImpl(final double t) {
        return originalCurve.zeroRate(
                t, Compounding.Continuous, Frequency.NoFrequency, true).rate() + spread.value();
    }

}
