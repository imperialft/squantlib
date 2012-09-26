package squantlib.task.pricing

import squantlib.database.schemadefinitions.{Coupon => dbCoupon}
import squantlib.database.objectconstructor.Coupon
import squantlib.database.DB
import scala.collection.immutable.StringLike
import scala.collection.mutable.{HashSet, SynchronizedSet, HashMap, SynchronizedMap}
import java.util.{Date => JavaDate}

object Coupons {
  
  var storedprice = new HashSet[dbCoupon] with SynchronizedSet[dbCoupon]
    
  def push:Unit = { 
    if (storedprice.size != 0) {
	    printf("Writing " + storedprice.size + " items to Database...")
		val t1 = System.nanoTime
		val result = DB.insertOrUpdate(storedprice, true)
		val t2 = System.nanoTime
		printf("done " + result + " records (%.3f sec)\n".format(((t2 - t1)/1000000000.0)))
		storedprice.clear
		}
	}
  
  def couponMissingBonds = DB.getCouponMissingBonds
  def unfixedCoupons(d:JavaDate) = DB.getUnfixedCoupons(d)
  def definedCoupons(d:JavaDate) = DB.getDefinedCoupons(d)
  
  def initialize = {
    storedprice ++= couponMissingBonds.map(_.getCoupons).flatten
  }
  
  def update(forceUpdate:Boolean):Unit = {
    if (storedprice.size > 0) push
    val d = DB.getLatestParamSet._2
    val updatecpns = if (forceUpdate) definedCoupons(d) else unfixedCoupons(d)
    printf("update " + updatecpns.size + " coupons..")
    val newcoupons = updatecpns.map(Coupon.update)
    printf("done")
    storedprice ++= newcoupons
  }
   
  def update:Unit = update(false)
  
}

