package net.squantlib.util.ql.daycounters;

import net.squantlib.util.ql.time.Date;

/**
 * Payment in absolute amount. ie. No daycount fraction.
 *
 * @author Masakatsu Wakayu
 * @category daycounters
 */
public class Absolute extends DayCounter {


  public Absolute() {
    super.impl = new Impl();
  }

  //
  // annual daycount (est)
  //

  public double annualDayCount() {
    return 1.0;
  }


  //
  // private inner classes
  //

  final private class Impl extends DayCounter.Impl {

    //
    // implements DayCounter
    //

    @Override
    public final String name() /* @ReadOnly */ {
      return "ABSOLUTE";
    }

    @Override
    public /*@Time*/ final double yearFraction(
      final Date dateStart,
      final Date dateEnd,
      final Date refPeriodStart,
      final Date refPeriodEnd
    ) /* @ReadOnly */ {
      return 1.00;
    }

  }


}
