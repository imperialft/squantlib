package net.squantlib.util.ql.indexes.ibor;

import net.squantlib.util.ql.currencies.Europe.HUFCurrency;
import net.squantlib.util.ql.daycounters.Actual360;
import net.squantlib.util.ql.termstructures.AbstractYieldTermStructure;
import net.squantlib.util.ql.termstructures.YieldTermStructure;
import net.squantlib.util.ql.Date;
import net.squantlib.util.ql.Period;
import net.squantlib.util.ql.calendars.Hungary;

/**
 * base class for all BBA LIBOR indexes but the EUR, O/N, and S/N ones
 * <p>
 * LIBOR fixed by BBA.
 *
 * @see <a href="http://www.bba.org.uk/bba/jsp/polopoly.jsp?d=225&a=1414">http://www.bba.org.uk/bba/jsp/polopoly.jsp?d=225&a=1414</a>
 */
public class HUFLibor extends Libor {

	public HUFLibor(final Period tenor) {
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

	public HUFLibor(final Period tenor,
			final YieldTermStructure h) {
		super("HUFLibor", tenor, 0,
				new HUFCurrency(),
				new Hungary(),
				new Actual360(), h);
	}

}
