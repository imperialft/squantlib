
/**
 * Creates factory from given paramset.
 * You must define following parameters in advance.
 *  val paramset:String => parameter id
 */

import squantlib.database.schemadefinitions.Coupon

val bonds = DB.getBonds
val coupons = bonds.map(_.getCoupons).filter(_ != null).flatten
//DB.insertOrReplace(coupons)

