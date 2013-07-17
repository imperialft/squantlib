package org.jquantlib.indexes.ibor;

import org.jquantlib.currencies.Europe.SEKCurrency;
import org.jquantlib.daycounters.Actual360;
import org.jquantlib.indexes.IborIndex;

import org.jquantlib.termstructures.AbstractYieldTermStructure;
import org.jquantlib.termstructures.YieldTermStructure;
import org.jquantlib.time.BusinessDayConvention;
import org.jquantlib.time.Date;
import org.jquantlib.time.Period;
import org.jquantlib.time.calendars.Sweden;;

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
