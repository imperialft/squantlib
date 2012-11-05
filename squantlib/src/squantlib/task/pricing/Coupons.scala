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
  
  def push(coupons:Set[dbCoupon]):Unit = { 
    if (coupons.size != 0) {
    	printf("Extracting valid price ..")
	    printf("Writing " + coupons.size + " items to Database...")
		val t1 = System.nanoTime
		DB.insertOrUpdate(coupons, true)
		val t2 = System.nanoTime
		printf("done (%.3f sec)\n".format(((t2 - t1)/1000000000.0)))
		}
	}
  
  def couponMissingBonds = DB.getCouponMissingBonds
  def unfixedCoupons(d:JavaDate) = DB.getUnfixedCoupons(d)
  def definedCoupons(d:JavaDate) = DB.getDefinedCoupons(d)
  
  def initialize = {
    storedprice ++= couponMissingBonds.map(Coupon(_)).flatten
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
  
  def update_par(forceUpdate:Boolean):Unit = {
    if (storedprice.size > 0) push
    val d = DB.getLatestParamSet._2
    val updatecpns = if (forceUpdate) definedCoupons(d) else unfixedCoupons(d)
    printf("update " + updatecpns.size + " coupons..")
    updatecpns.par.foreach{ c => {
      val newcoupon = Coupon.update(c)
      push(Set(newcoupon))
    }}
    printf("done")
  }
   
  def update:Unit = update(false)
  
}

