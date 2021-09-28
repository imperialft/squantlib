/*
 Copyright (C) 2008 Srinivas Hasti

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

package net.squantlib.util.ql.indexes;

import net.squantlib.util.ql.time.TimeSeries;
import net.squantlib.util.ql.util.Observable;
import net.squantlib.util.ql.util.ObservableValue;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;


public class IndexManager {

  private static final long serialVersionUID = -9204254124065694863L;

  private static Map<String, TimeSeries<Double>> data;
  private static volatile IndexManager instance;


  //
  // static public methods
  //

  public static IndexManager getInstance() {
    if (instance == null) {
      synchronized (IndexManager.class) {
        if (instance == null) {
          instance = new IndexManager();
        }
      }
    }
    return instance;
  }


  //
  // private constructors
  //

  private IndexManager() {
    this.data = new ConcurrentHashMap<String, TimeSeries<Double>>();
  }

  public TimeSeries<Double> getHistory(final String name) {
    return data.get(name);
  }

  public void setHistory(
    final String name,
    final TimeSeries<Double> history
  ) {
    data.put(name, history);
  }

  public void clearHistory(final String name) {
    data.remove(name);
  }

  public void clearHistories() {
    data.clear();
  }

  public Observable notifier(final String name) {
    TimeSeries<Double> value = data.get(name);
    if (value == null) {
      value = new TimeSeries<Double>(Double.class);
      data.put(name, value);
    }
    return new ObservableValue<TimeSeries<Double>>(value);
  }

}
