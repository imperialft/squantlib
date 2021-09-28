package net.squantlib.util.ql.indexes.ibor;

import net.squantlib.util.ql.currencies.Europe.SEKCurrency;
import net.squantlib.util.ql.daycounters.Actual360;
import net.squantlib.util.ql.indexes.IborIndex;
import net.squantlib.util.ql.termstructures.AbstractYieldTermStructure;
import net.squantlib.util.ql.termstructures.YieldTermStructure;
import net.squantlib.util.ql.time.BusinessDayConvention;
import net.squantlib.util.ql.time.Date;
import net.squantlib.util.ql.time.Period;
import net.squantlib.util.ql.time.calendars.Sweden;

;

/**
 * Stockholm Interbank Offered Rate. 
 * This is the rate fixed in Stockholm is compiled by NASDAQ OMX Stockholm.
 * 
 * TODO check settlement days, end-of-month adjustment, and day-count convention.
 * 
 */
public class STIBOR extends IborIndex {

	public STIBOR(final Period tenor) {
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

	public STIBOR(final Period tenor,
			final YieldTermStructure h) {
		super("STIBOR", tenor, 2,
				new SEKCurrency(),
				new Sweden(),
				BusinessDayConvention.ModifiedFollowing,
				false,
				new Actual360(), 
				h);
	}

}
