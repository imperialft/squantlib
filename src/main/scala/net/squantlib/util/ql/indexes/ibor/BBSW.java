package net.squantlib.util.ql.indexes.ibor;

import net.squantlib.util.ql.currencies.Oceania.AUDCurrency;
import net.squantlib.util.ql.daycounters.Actual365Fixed;
import net.squantlib.util.ql.indexes.IborIndex;
import net.squantlib.util.ql.termstructures.AbstractYieldTermStructure;
import net.squantlib.util.ql.termstructures.YieldTermStructure;
import net.squantlib.util.ql.BusinessDayConvention;
import net.squantlib.util.ql.Date;
import net.squantlib.util.ql.Period;
import net.squantlib.util.ql.calendars.Australia;

/**
 * Australian Dollar bank bill rate published by AFMA.
 * See <http://www.afma.com.au>.
 *        
 * TODO check settlement days, end-of-month adjustment, and day-count convention.
 */
public class BBSW extends IborIndex {

	public BBSW(final Period tenor) {
		this(tenor, 
						new AbstractYieldTermStructure() {
							@Override
							protected double discountImpl(final double t) {
								throw new UnsupportedOperationException();
							}
							@Override
							public Date maxDate() {
								throw new UnsupportedOperationException();
							}
						}
				);
	}

	public BBSW(final Period tenor,
                final YieldTermStructure h) {
		super("BBSW", tenor, 2,
				new AUDCurrency(),
				new Australia(),
				BusinessDayConvention.ModifiedFollowing,
				false,
				new Actual365Fixed(), 
				h);
	}

}
