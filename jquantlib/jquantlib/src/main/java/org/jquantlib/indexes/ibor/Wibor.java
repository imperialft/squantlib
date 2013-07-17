package org.jquantlib.indexes.ibor;

import org.jquantlib.currencies.Europe.PLNCurrency;
import org.jquantlib.daycounters.Actual360;
import org.jquantlib.indexes.IborIndex;

import org.jquantlib.termstructures.AbstractYieldTermStructure;
import org.jquantlib.termstructures.YieldTermStructure;
import org.jquantlib.time.BusinessDayConvention;
import org.jquantlib.time.Date;
import org.jquantlib.time.Period;
import org.jquantlib.time.calendars.Poland;;

/**
 * Warsaw Interbank Offered Rate. 
 * This is the rate fixed in Warsaw by National Bank of Poland
 * 
 * TODO check settlement days, end-of-month adjustment, and day-count convention.
 * 
 */
public class Wibor extends IborIndex {

	public Wibor(final Period tenor) {
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

	public Wibor(final Period tenor,
			final YieldTermStructure h) {
		super("Wibor", tenor, 2,
				new PLNCurrency(),
				new Poland(),
				BusinessDayConvention.ModifiedFollowing,
				false,
				new Actual360(), 
				h);
	}

}
