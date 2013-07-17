package org.jquantlib.indexes.ibor;

import org.jquantlib.currencies.Oceania.NZDCurrency;
import org.jquantlib.daycounters.Actual365Fixed;
import org.jquantlib.indexes.IborIndex;

import org.jquantlib.termstructures.AbstractYieldTermStructure;
import org.jquantlib.termstructures.YieldTermStructure;
import org.jquantlib.time.BusinessDayConvention;
import org.jquantlib.time.Date;
import org.jquantlib.time.Period;
import org.jquantlib.time.calendars.NewZealand;

/**
 * New Zealand Dollar bank bill rate published by NZFMA.
 * See <http://www.nzfma.org>.
 *        
 * TODO check settlement days, end-of-month adjustment, and day-count convention.
 */
public class BKBM extends IborIndex {

	public BKBM(final Period tenor) {
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

	public BKBM(final Period tenor,
			final YieldTermStructure h) {
		super("BKBM", tenor, 2,
				new NZDCurrency(),
				new NewZealand(),
				BusinessDayConvention.ModifiedFollowing,
				false,
				new Actual365Fixed(), 
				h);
	}

}
