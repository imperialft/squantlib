package net.squantlib.util.ql.indexes.ibor;

import net.squantlib.util.ql.currencies.Oceania.NZDCurrency;
import net.squantlib.util.ql.daycounters.Actual365Fixed;
import net.squantlib.util.ql.indexes.IborIndex;
import net.squantlib.util.ql.termstructures.AbstractYieldTermStructure;
import net.squantlib.util.ql.termstructures.YieldTermStructure;
import net.squantlib.util.ql.BusinessDayConvention;
import net.squantlib.util.ql.Date;
import net.squantlib.util.ql.Period;
import net.squantlib.util.ql.calendars.NewZealand;

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
