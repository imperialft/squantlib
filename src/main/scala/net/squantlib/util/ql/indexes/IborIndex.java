/*
 Copyright (C) 2008 Srinivas Hasti

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
 Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008 StatPro Italia srl

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

package net.squantlib.util.ql.indexes;


import net.squantlib.util.ql.QL;
import net.squantlib.util.ql.currencies.Currency;
import net.squantlib.util.ql.daycounters.DayCounter;
import net.squantlib.util.ql.lang.exceptions.LibraryException;
import net.squantlib.util.ql.termstructures.YieldTermStructure;
import net.squantlib.util.ql.BusinessDayConvention;
import net.squantlib.util.ql.Calendar;
import net.squantlib.util.ql.Date;
import net.squantlib.util.ql.Period;

/**
 * Base class for Inter-Bank-Offered-Rate indexes (e.g. %Libor, etc.)
 *
 * @author Srinivas Hasti
 * @author Zahid Hussain
 */
// TODO: code review :: license, class comments, comments for access modifiers, comments for @Override
public class IborIndex extends InterestRateIndex {

    private final BusinessDayConvention convention;
    private final YieldTermStructure termStructure;
    private final boolean endOfMonth;

    public IborIndex(
            final String familyName,
            final Period tenor,
            final /*@Natural*/ int fixingDays,
            final Currency currency,
            final Calendar fixingCalendar,
            final BusinessDayConvention convention,
            final boolean endOfMonth,
            final DayCounter dayCounter,
            final YieldTermStructure h) {
        super(familyName, tenor, fixingDays, currency, fixingCalendar, dayCounter);

        this.convention = convention;
        this.termStructure = h;
        this.endOfMonth = endOfMonth;
        if (termStructure != null) {
        	termStructure.addObserver(this);
        }
    }

    public IborIndex(
            final String familyName,
            final Period tenor,
            final /*@Natural*/ int fixingDays,
            final Currency currency,
            final Calendar fixingCalendar,
            final BusinessDayConvention convention,
            final boolean endOfMonth,
            final DayCounter dayCounter) {
    	this(familyName, tenor, fixingDays, currency, fixingCalendar, 
    		convention, endOfMonth, dayCounter, null);
    }


    public IborIndex clone(final YieldTermStructure h) {
        final IborIndex clone = new IborIndex(
                					this.familyName(),
                					this.tenor(),
                					this.fixingDays(),
                					this.currency(),
                					this.fixingCalendar(),
                					this.businessDayConvention(),
                					this.endOfMonth(),
                					this.dayCounter(),
                					h);
        return clone ;
    }


    //
    // protected methods
    //
    
    /**
     * This is the fixing established by ECB.
     * Use eurliborConvention if you're interested in the London rate fixed by the BBA.
     */
    protected static BusinessDayConvention euriborConvention(final Period p) {
        switch (p.units()) {
        case Days:
        case Weeks:
            return BusinessDayConvention.Following;
        case Months:
        case Years:
            return BusinessDayConvention.ModifiedFollowing;
        default:
            throw new LibraryException("invalid time units"); // TODO: message
        }
    }

    /**
     * This is the London fixing by BBA.
     * Use euriborConvention if you're interested in the rate fixed by the ECB.
     */
    protected static BusinessDayConvention eurliborConvention(final Period p) {
        switch (p.units()) {
        case Days:
        case Weeks:
            return BusinessDayConvention.Following;
        case Months:
        case Years:
            return BusinessDayConvention.ModifiedFollowing;
        default:
            throw new LibraryException("invalid time units"); // TODO: message
        }
    }
    

    /**
     * This is the fixing established by ECB.
     * Use eurliborEOM if you're interested in the London rate fixed by the BBA.
     */
    protected static boolean euriborEOM(final Period p) {
        switch (p.units()) {
        case Days:
        case Weeks:
            return false;
        case Months:
        case Years:
            return true;
        default:
            throw new LibraryException("invalid time units");  // TODO: message
        }
    }

    /**
     * This is the London fixing by BBA.
     * Use euriborConvention if you're interested in the rate fixed by the ECB.
     */
    protected static boolean eurliborEOM(final Period p) {
        switch (p.units()) {
        case Days:
        case Weeks:
            return false;
        case Months:
        case Years:
            return true;
        default:
            throw new LibraryException("invalid time units");  // TODO: message
        }
    }


    protected static BusinessDayConvention liborConvention(final Period p) {
        switch (p.units()) {
	        case Days:
	        case Weeks:
	            return BusinessDayConvention.Following;
	        case Months:
	        case Years:
	            return BusinessDayConvention.ModifiedFollowing;
	        default:
	            throw new LibraryException("invalid time units"); // TODO: message
        }
    }

    protected static boolean liborEOM(final Period p) {
        switch (p.units()) {
	        case Days:
	        case Weeks:
	            return false;
	        case Months:
	        case Years:
	            return true;
	        default:
	            throw new LibraryException("invalid time units"); // TODO: message
        }
    }

    
    //
    // public methods
    //
    
    public BusinessDayConvention businessDayConvention() {
        return convention;
    }

    public boolean endOfMonth() {
        return endOfMonth;
    }


    //
    // overrides InterestRateIndex
    //
    
    @Override
    protected double forecastFixing(final Date fixingDate) {
        QL.require(termStructure != null , "no forecasting term structure set to " + name());  // TODO: message
        final Date fixingValueDate = valueDate(fixingDate);
        final Date endValueDate = maturityDate(fixingValueDate);
        final double fixingDiscount = termStructure.discount(fixingValueDate);
        final double endDiscount = termStructure.discount(endValueDate);
        final double fixingPeriod = dayCounter().yearFraction(fixingValueDate, endValueDate);
        return (fixingDiscount / endDiscount - 1.0) / fixingPeriod;
    }

    @Override
    public YieldTermStructure termStructure() {
        return termStructure;
    }

    @Override
    public Date maturityDate(final Date valueDate) {
        return fixingCalendar().advance(valueDate, tenor, convention, endOfMonth);
    }

}
