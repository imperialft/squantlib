/*
 Copyright (C) 2008 Richard Gomes

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

package net.squantlib.util.ql.math.interpolations;

/**
 * This class is intended to implement the default behavior of an Extrapolator.
 *
 * @author Richard Gomes
 */
public class DefaultExtrapolator implements Extrapolator {

	//
    // private fields
    //

    private boolean extrapolate;


    //
    // public constructors
    //

    /**
     * @category constructors
     */
    public DefaultExtrapolator() {
		this.extrapolate = false;
	}


	//
	// implements Extrapolator
	//

    @Override
    public void enableExtrapolation() {
		extrapolate = true;
	}

    @Override
	public void disableExtrapolation() {
		extrapolate = false;
	}

    @Override
	public final boolean allowsExtrapolation() {
		return extrapolate;
	}

}
