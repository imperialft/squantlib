/*
 Copyright (C) 2011 Tim Blackler

 This source code is release under the BSD License.

 This file is part of JQuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://jquantlib.org/

 JQuantLib is free software: you can redistribute it and/or modify it
 under the terms of the JQuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <jquant-devel@lists.sourceforge.net>. The license is also available online at
 <http://www.jquantlib.org/index.php/LICENSE.TXT>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.

 JQuantLib is based on QuantLib. http://quantlib.org/
 When applicable, the original copyright notice follows this notice.
 */
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006 StatPro Italia srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

package net.squantlib.util.ql.indexes.ibor;

import net.squantlib.util.ql.currencies.Asia.JPYCurrency;
import net.squantlib.util.ql.daycounters.Actual365Fixed;
import net.squantlib.util.ql.indexes.IborIndex;
import net.squantlib.util.ql.termstructures.AbstractYieldTermStructure;
import net.squantlib.util.ql.termstructures.YieldTermStructure;
import net.squantlib.util.ql.BusinessDayConvention;
import net.squantlib.util.ql.Date;
import net.squantlib.util.ql.Period;
import net.squantlib.util.ql.calendars.Japan;

/**
 * Tokyo Interbank Offered Rate
 * This is the rate fixed in Tokio by JBA. 
 * Use JPYLibor if you're interested in the London fixing by BBA.
 * 
 * TODO check settlement days and end-of-month adjustment.
 
 */
public class Tibor extends IborIndex {

	public Tibor(final Period tenor) {
		this(tenor, new AbstractYieldTermStructure() {
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

	public Tibor(final Period tenor,
			final YieldTermStructure h) {
		super("Tibor", tenor, 0,
				new JPYCurrency(),
				new Japan(),
				BusinessDayConvention.ModifiedFollowing,
				false,
				new Actual365Fixed(), 
				h);
	}

}
