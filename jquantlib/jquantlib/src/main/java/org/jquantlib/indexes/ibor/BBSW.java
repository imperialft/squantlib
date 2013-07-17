package org.jquantlib.indexes.ibor;

import org.jquantlib.currencies.Oceania.AUDCurrency;
import org.jquantlib.daycounters.Actual365Fixed;
import org.jquantlib.indexes.IborIndex;

import org.jquantlib.termstructures.AbstractYieldTermStructure;
import org.jquantlib.termstructures.YieldTermStructure;
import org.jquantlib.time.BusinessDayConvention;
import org.jquantlib.time.Date;
import org.jquantlib.time.Period;
import org.jquantlib.time.calendars.Australia;

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
