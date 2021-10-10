package net.squantlib.util.initializer

import net.squantlib.util.ql.currencies.Africa._
import net.squantlib.util.ql.currencies.Asia._
import net.squantlib.util.ql.currencies.America._
import net.squantlib.util.ql.currencies.Europe._
import net.squantlib.util.ql.currencies.Oceania._
import net.squantlib.util.ql.currencies.Currency

object Currencies extends Initializer[Currency]{
  
	val mapper = Map(
			("ARS" -> new ARSCurrency),
			("ATS" -> new ATSCurrency),
			("AUD" -> new AUDCurrency),
			("BDT" -> new BDTCurrency),
			("BEF" -> new BEFCurrency),
			("BGL" -> new BGLCurrency),
			("BRL" -> new BRLCurrency),
			("BYR" -> new BYRCurrency),
			("CAD" -> new CADCurrency),
			("CHF" -> new CHFCurrency),
			("CLP" -> new CLPCurrency),
			("CNY" -> new CNYCurrency),
			("COP" -> new COPCurrency),
			("CYP" -> new CYPCurrency),
			("CZK" -> new CZKCurrency),
			("DEM" -> new DEMCurrency),
			("DKK" -> new DKKCurrency),
			("EEK" -> new EEKCurrency),
			("ESP" -> new ESPCurrency),
			("EUR" -> new EURCurrency),
			("FIM" -> new FIMCurrency),
			("FRF" -> new FRFCurrency),
			("GBP" -> new GBPCurrency),
			("GRD" -> new GRDCurrency),
			("HKD" -> new HKDCurrency),
			("HUF" -> new HUFCurrency),
			("IDR" -> new IDRCurrency),
			("IEP" -> new IEPCurrency),
			("ILS" -> new ILSCurrency),
			("INR" -> new INRCurrency),
			("IQD" -> new IQDCurrency),
			("IRR" -> new IRRCurrency),
			("ISK" -> new ISKCurrency),
			("ITL" -> new ITLCurrency),
			("JPY" -> new JPYCurrency),
			("KRW" -> new KRWCurrency),
			("KWD" -> new KWDCurrency),
			("LTL" -> new LTLCurrency),
			("LUF" -> new LUFCurrency),
			("LVL" -> new LVLCurrency),
			("MTL" -> new MTLCurrency),
			("MXN" -> new MXNCurrency),
			("NLG" -> new NLGCurrency),
			("NOK" -> new NOKCurrency),
			("NPR" -> new NPRCurrency),
			("NZD" -> new NZDCurrency),
			("PEH" -> new PEHCurrency),
			("PEI" -> new PEICurrency),
			("PEN" -> new PENCurrency),
			("PKR" -> new PKRCurrency),
			("PLN" -> new PLNCurrency),
			("PTE" -> new PTECurrency),
			("ROL" -> new ROLCurrency),
			("RON" -> new RONCurrency),
			("RUB" -> new RUBCurrency),
			("SAR" -> new SARCurrency),
			("SEK" -> new SEKCurrency),
			("SGD" -> new SGDCurrency),
			("SIT" -> new SITCurrency),
			("SKK" -> new SKKCurrency),
			("THB" -> new THBCurrency),
			("TRL" -> new TRLCurrency),
			("TRY" -> new TRYCurrency),
			("TTD" -> new TTDCurrency),
			("TWD" -> new TWDCurrency),
			("USD" -> new USDCurrency),
			("VEB" -> new VEBCurrency),
			("ZAR" -> new ZARCurrency)
	)

	def isForex(s:String):Boolean = {
		if (s.size == 6) contains(s.take(3)) && contains(s.takeRight(3))
		else false
	}

}

