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

package org.jquantlib.cashflow;

import org.jquantlib.QL;

import org.jquantlib.termstructures.SwaptionVolatilityStructure;

/**
 * Base pricer for vanilla CMT coupons
 *
 * @author Masakatsu Wakayu
 */
// TODO: code review :: license, class comments, comments for access modifiers, comments for @Override
public abstract class CmtCouponPricer extends FloatingRateCouponPricer {

    private SwaptionVolatilityStructure bondOptionVol_;
    private static final String no_adequate_swaptionVol_given = "no adequate swaptionVol given";

    public CmtCouponPricer() {
    }
    
    public CmtCouponPricer(final SwaptionVolatilityStructure bondOptionVol) {
        this.bondOptionVol_ = bondOptionVol;
        this.bondOptionVol_.addObserver(this);
        //XXX:registerWith
        //registerWith(this.swaptionVol_);
    }

    public SwaptionVolatilityStructure bondOptionVolatility() {
        return bondOptionVol_;
    }

    public void setBondOptionVolatility(final SwaptionVolatilityStructure bondOptionVol) {
    	bondOptionVol.deleteObserver(this);
        //XXX:registerWith
        //unregisterWith(swaptionVol);

        this.bondOptionVol_ = bondOptionVol;
        QL.require(bondOptionVol_!=null && bondOptionVol_ != null , no_adequate_swaptionVol_given); // TODO: message

        this.bondOptionVol_.addObserver(this);
        //registerWith(swaptionVol_);
        update();
    }


    //
    // implements Observer
    //

    @Override
    //TODO: code review
    public void update() {
        notifyObservers();
    }

}
