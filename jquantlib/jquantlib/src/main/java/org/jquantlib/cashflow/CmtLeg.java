/*
Copyright (C) 2009 Ueli Hofstetter
Copyright (C) 2009 John Martin

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
 Copyright (C) 2006 Giorgio Facchinetti
 Copyright (C) 2006 Mario Pucci
 Copyright (C) 2006, 2007 StatPro Italia srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.


 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the license for more details.
*/

package org.jquantlib.cashflow;

import org.jquantlib.daycounters.DayCounter;
import org.jquantlib.indexes.BondYieldIndex;
import org.jquantlib.math.matrixutilities.Array;
import org.jquantlib.time.BusinessDayConvention;
import org.jquantlib.time.Schedule;

/**
 * Helper class building a sequence of capped/floored cmt-rate coupons
 *
 * @author Masakatsu Wakayu
 */
public class CmtLeg {

    private final Schedule schedule_;
    private final BondYieldIndex bondYieldIndex_;
    private/* @Real */Array notionals_;
    private DayCounter paymentDayCounter_;
    private BusinessDayConvention paymentAdjustment_;
    private Array fixingDays_;
    private Array gearings_;
    private/* @Spread */Array spreads_;
    private/* @Rate */Array caps_;
    private Array floors_;
    private boolean inArrears_;
    private boolean zeroPayments_;


    public CmtLeg(final Schedule schedule, final BondYieldIndex bondYieldIndex) {
        schedule_ = schedule;
        bondYieldIndex_ = bondYieldIndex;
        paymentAdjustment_ = BusinessDayConvention.Following;
        inArrears_ = false;
        zeroPayments_ = false;
        
        fixingDays_ = new Array(0);
        gearings_ = new Array(0);
        spreads_ = new Array(0);
        caps_ = new Array(0);
        floors_ = new Array(0);
    }


    public CmtLeg withNotionals(/* Real */final double notional) {
        notionals_ = new Array(1).fill(notional);
        return this;
    }

    public CmtLeg withNotionals(final Array notionals) {
        notionals_ = notionals;
        return this;
    }

    
    public CmtLeg withPaymentDayCounter(final DayCounter dayCounter) {
        paymentDayCounter_ = dayCounter;
        return this;
    }

    public CmtLeg withPaymentAdjustment(final BusinessDayConvention convention) {
        paymentAdjustment_ = convention;
        return this;
    }
    
    public CmtLeg withFixingDays(/* Natural */final int fixingDays) {
        fixingDays_ = new Array(1).fill(fixingDays);
        return this;
    }

    public CmtLeg withFixingDays(final Array fixingDays) {
        fixingDays_ = fixingDays.clone();
        return this;
    }

    public CmtLeg withGearings(/* Real */final double gearing) {
        gearings_ = new Array(1).fill(gearing);
        return this;
    }

    public CmtLeg withGearings(final Array gearings) {
        gearings_ = gearings.clone();
        return this;
    }

    public CmtLeg withSpreads(/* Spread */final double spread) {
        spreads_ = new Array(1).fill(spread);
        return this;
    }

    public CmtLeg withSpreads(final Array spreads) {
        spreads_ = spreads.clone();
        return this;
    }

    public CmtLeg withCaps(/* @Rate */final double cap) {
        caps_ = new Array(1).fill(cap);
        return this;
    }

    public CmtLeg withCaps(final Array caps) {
        caps_ = caps.clone();
        return this;
    }

    public CmtLeg withFloors(/* @Rate */final double floor) {
        floors_ = new Array(1).fill(floor);
        return this;
    }

    public CmtLeg withFloors(final Array floors) {
        floors_ = floors.clone();
        return this;
    }

    public CmtLeg inArrears(final boolean flag) {
        inArrears_ = flag;
        return this;
    }

    public CmtLeg withZeroPayments(final boolean flag) {
        zeroPayments_ = flag;
        return this;
    }

    public Leg Leg() {
        return new FloatingLeg<BondYieldIndex, CmtCoupon, CappedFlooredCmtCoupon> (
        		BondYieldIndex.class, CmtCoupon.class, CappedFlooredCmtCoupon.class,
        		notionals_, schedule_, bondYieldIndex_, paymentDayCounter_,
       			paymentAdjustment_, fixingDays_, gearings_, spreads_,
       			caps_, floors_, inArrears_, zeroPayments_);
    }

}
