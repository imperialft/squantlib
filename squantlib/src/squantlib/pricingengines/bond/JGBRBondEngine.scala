package squantlib.pricingengines.bond

import squantlib.database.DB

import org.jquantlib.QL
import org.jquantlib.instruments.Bond
import org.jquantlib.time.{Date => qlDate}

class JGBRBondEngine(val valuationDate:qlDate) extends Bond.EngineImpl {
  
    override def calculate() /* @ReadOnly */ = {
        val a = arguments_.asInstanceOf[Bond.ArgumentsImpl]
        var r = results_.asInstanceOf[Bond.ResultsImpl]
        val vd = valuationDate.longDate
        val (currentcpns, lastcpns) = DB.getCurrentAndPreviousCoupons(a.bondid, vd, 2)
        
        val accrued = currentcpns.map(_.accruedCoupon(vd).getOrElse(Double.NaN)).sum
        val previous = lastcpns.map(_.fixedamount.getOrElse(Double.NaN)).sum

        r.value = (1 + accrued - previous)*100
        r.settlementValue = r.value
    }

}
