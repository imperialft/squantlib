package net.squantlib.util.ql.indexes.ibor;

import net.squantlib.util.ql.currencies.Europe.PLNCurrency;
import net.squantlib.util.ql.daycounters.Actual360;
import net.squantlib.util.ql.indexes.IborIndex;
import net.squantlib.util.ql.termstructures.AbstractYieldTermStructure;
import net.squantlib.util.ql.termstructures.YieldTermStructure;
import net.squantlib.util.ql.time.BusinessDayConvention;
import net.squantlib.util.ql.time.Date;
import net.squantlib.util.ql.time.Period;
import net.squantlib.util.ql.time.calendars.Poland;

;

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
