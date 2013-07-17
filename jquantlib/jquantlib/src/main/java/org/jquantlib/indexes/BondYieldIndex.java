package org.jquantlib.indexes;

import org.jquantlib.currencies.Currency;
import org.jquantlib.daycounters.DayCounter;
import org.jquantlib.instruments.Bond;

import org.jquantlib.termstructures.YieldTermStructure;
import org.jquantlib.time.BusinessDayConvention;
import org.jquantlib.time.Calendar;
import org.jquantlib.time.Date;
import org.jquantlib.time.Period;

/**
 * Base class for swap-rate indexes
 *
 * @author Richard Gomes
 */
// TODO: code review :: license, class comments, comments for access modifiers, comments for @Override
public class BondYieldIndex extends InterestRateIndex {

    //
    // protected fields
    //

    protected Period tenor;
    protected Period fixedLegTenor;
    protected BusinessDayConvention fixedLegConvention;


    //
    // public constructors
    //

    public BondYieldIndex(
            final String familyName,
            final Period tenor,
            final /*@Natural*/ int settlementDays,
            final Currency currency,
            final Calendar calendar,
            final Period fixedLegTenor,
            final BusinessDayConvention fixedLegConvention,
            final DayCounter fixedLegDayCounter) {
        super(familyName, tenor, settlementDays, currency, calendar, fixedLegDayCounter);
        this.tenor = tenor;
        this.fixedLegTenor = fixedLegTenor;
        this.fixedLegConvention = fixedLegConvention;
        
        //XXX:registerWith
        //registerWith(this.iborIndex);
    }


    //
    // protected methods
    //

    @Override
    protected /*@Rate*/ double forecastFixing(final Date fixingDate) /* @ReadOnly */ {
		throw new UnsupportedOperationException();    	
    }


    //
    // public methods
    //

    public Period fixedLegTenor() /* @ReadOnly */{
        return fixedLegTenor;
    }

    public BusinessDayConvention fixedLegConvention() /* @ReadOnly */ {
        return fixedLegConvention;
    }

    public Bond underlyingBond(final Date fixingDate) /* @ReadOnly */ {
		throw new UnsupportedOperationException();    	
    }

    @Override
    public Date maturityDate(final Date valueDate) /* @ReadOnly */ {
        final Date fixDate = fixingDate(valueDate);
        return fixingCalendar().advance(fixDate, tenor);
    }



    @Override
    public YieldTermStructure termStructure() /* @ReadOnly */ {
		throw new UnsupportedOperationException();    	
    }

}
