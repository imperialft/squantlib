package squantlib.jquantlib.cashflow

import org.jquantlib.cashflow.CappedFlooredCmtCoupon
import org.jquantlib.cashflow.CmtCouponPricer
import org.jquantlib.termstructures.SwaptionVolatilityStructure
import org.jquantlib.cashflow.FloatingRateCoupon
import squantlib.model.yieldparameter.YieldParameter
import org.jquantlib.time.{Date => qlDate}
 
class JGBRFloatingCouponPricer(var coupon:CappedFlooredCmtCoupon, val fixing:qlDate => Double) extends CmtCouponPricer{

    override def swapletPrice():Double = 
      throw new java.lang.UnsupportedOperationException("pricing on CMT not implemented")
    
    override def swapletRate():Double = coupon.gearing * fixing(coupon.fixingDate) + coupon.spread
	
    override def capletPrice(effectiveCap:Double):Double = 
      throw new java.lang.UnsupportedOperationException("option pricing on CMT not implemented")

    override def capletRate(effectiveCap:Double):Double = Math.max(0, swapletRate - effectiveCap)
    
    override def floorletPrice(effectiveFloor:Double):Double = 
      throw new java.lang.UnsupportedOperationException("option pricing on CMT not implemented")
    
    override def floorletRate(effectiveFloor:Double):Double = Math.max(0, effectiveFloor - swapletRate)
    
    override def initialize(cpn:FloatingRateCoupon):Unit = {
      cpn match {
        case c:CappedFlooredCmtCoupon => this.coupon = c
        case _ => {}
      }
	}
    
    def this(fixing:qlDate => Double) = this(null, fixing)
  
}
