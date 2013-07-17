package org.jquantlib.indexes.ibor;

import org.jquantlib.currencies.Europe.HUFCurrency;
import org.jquantlib.daycounters.Actual360;

import org.jquantlib.termstructures.AbstractYieldTermStructure;
import org.jquantlib.termstructures.YieldTermStructure;
import org.jquantlib.time.Date;
import org.jquantlib.time.Period;
import org.jquantlib.time.calendars.Hungary;

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
