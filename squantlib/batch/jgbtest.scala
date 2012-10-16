import squantlib.database.objectconstructor.BondPrice
import org.jquantlib.time.{Date => qlDate, Frequency }
import org.jquantlib.pricingengines.PricingEngine
import org.jquantlib.daycounters.Thirty360
import org.jquantlib.instruments.{Bond => QLBond}
import squantlib.database.schemadefinitions.{BondPrice => dbBondPrice}
import org.jquantlib.termstructures.{YieldTermStructure, Compounding}
import org.jquantlib.cashflow.CashFlows
import java.util.{Date => javaDate}
import squantlib.model.discountcurve.DiscountCurveFactory
import squantlib.cashflow._
import scala.collection.JavaConversions._
import org.jquantlib.cashflow.CappedFlooredCmtCoupon

val factory = QLDB.getDiscountCurveFactory("20110308-000").orNull
val bond = QLDB.getBond("JGBR-0056", factory).orNull

val stddaycount = new org.jquantlib.daycounters.Thirty360

//bond.`yield`(stddaycount, Compounding.Compounded, Frequency.Annual)
//
val cfmodel = CashFlows.getInstance
val cashflows = bond.cashflows
//
val valuedate = factory.valuedate
val price = bond.dirtyPrice
//
//val fixing = (x:Date) => 0.05
//val pricer = new JGBRFloatingCouponPricer(fixing)
//
//bond.cashflows.foreach{case c:CappedFlooredCmtCoupon => {c.setPricer(pricer); println("pricer set");} case _ => {println("no pricer")} }
//
//val cmtcpn = bond.cashflows.first.asInstanceOf[CappedFlooredCmtCoupon]
//val cmtp = cmtcpn.pricer.asInstanceOf[JGBRFloatingCouponPricer]

//cfmodel.irr(cashflows, price, new Thirty360, Compounding.Continuous, Frequency.NoFrequency, valuedate, 0.001, 1000, 0.01)

//val vd = (new Date(5, 5, 2012)).longDate
