import org.specs2.mutable._
import org.jquantlib.currencies._

class CurrenciesSpec extends Specification {
  val subclasses = List(
    new Asia.BDTCurrency,
    new Asia.CNYCurrency,
    new Asia.HKDCurrency,
    new Asia.ILSCurrency,
    new Asia.INRCurrency,
    new Asia.IQDCurrency,
    new Asia.IRRCurrency,
    new Asia.JPYCurrency,
    new Asia.KRWCurrency,
    new Asia.KWDCurrency,
    new Asia.NPRCurrency,
    new Asia.PKRCurrency,
    new Asia.SARCurrency,
    new Asia.SGDCurrency,
    new Asia.THBCurrency,
    new Asia.TWDCurrency,
    new Europe.BGLCurrency,
    new Europe.BYRCurrency,
    new Europe.CHFCurrency,
    new Europe.CYPCurrency,
    new Europe.CZKCurrency,
    new Europe.DKKCurrency,
    new Europe.EEKCurrency,
    new Europe.EURCurrency,
    new Europe.GBPCurrency,
    new Europe.HUFCurrency,
    new Europe.ISKCurrency,
    new Europe.LTLCurrency,
    new Europe.LVLCurrency,
    new Europe.MTLCurrency,
    new Europe.NOKCurrency,
    new Europe.PLNCurrency,
    new Europe.ROLCurrency,
    new Europe.RONCurrency,
    new Europe.SEKCurrency,
    new Europe.SITCurrency,
    new Europe.SKKCurrency,
    new Europe.TRLCurrency,
    new Europe.TRYCurrency,
    new Europe.ATSCurrency,
    new Europe.BEFCurrency,
    new Europe.DEMCurrency,
    new Europe.ESPCurrency,
    new Europe.FIMCurrency,
    new Europe.FRFCurrency,
    new Europe.GRDCurrency,
    new Europe.IEPCurrency,
    new Europe.ITLCurrency,
    new Europe.LUFCurrency,
    new Europe.NLGCurrency,
    new Europe.PTECurrency,
    new America.ARSCurrency,
    new America.BRLCurrency,
    new America.CADCurrency,
    new America.CLPCurrency,
    new America.COPCurrency,
    new America.MXNCurrency,
    new America.PENCurrency,
    new America.PEICurrency,
    new America.PEHCurrency,
    new America.TTDCurrency,
    new America.USDCurrency,
    new America.VEBCurrency,
    new Africa.ZARCurrency,
    new Oceania.AUDCurrency,
    new Oceania.NZDCurrency
  )

  subclasses foreach { klass => klass.toString in { examples(klass) } }

  def examples(currency:Currency) = {
    "#name" should {
      "must not be empty" in {
        currency.name must not be empty
      }
    }
    "#code" should {
      "must be matching /^[A-Z]{3}$/" in {
        currency.code must be matching "^[A-Z]{3}$".r
      }
    }
    "#toString" should {
      "must be equal to #code" in {
        currency.code must be equalTo currency.toString
      }
    }
    "#numericCode" should {
      "must be greater than 0 and less than 1000" in {
        currency.numericCode must beGreaterThan(0)
        currency.numericCode must beLessThan(1000)
      }
    }
    "#symbol" should {
      "must not be empty" in {
        currency.symbol must not be empty
      }
    }
    "#fractionSymbol" should {
      "must not be empty" in {
        currency.fractionSymbol must not be empty
      }
    }
    "#fractionsPerUnit" should {
      "must be greater than 0" in {
        currency.fractionsPerUnit must be greaterThan(0)
      }
      "must be 1 or divided by 10 without subsidue" in {
        if (currency.fractionsPerUnit == 1 || currency.fractionsPerUnit % 10 == 0)
          success
        else
          failure
      }
    }
    "#format" should {
      "must not be empty" in {
        currency.format must not be empty
      }
      "must be format string" in {
        currency.format must be matching "^(\\d|f|%|\\s|\\$|\\.)+$".r
      }
    }
    "#empty" should {
      "must be false" in {
        currency.empty must beFalse
      }
    }
    "#triangulationCurrency" should {
      todo
    }
    "#eq" should {
      "must be true" in {
        currency.eq(currency) must beTrue
      }
      "must be false" in {
        currency.eq((new Currency)) must beFalse
      }
    }
    "#ne" should {
      "must be true" in {
        currency.ne((new Currency)) must beTrue
      }
      "must be false" in {
        currency.ne(currency) must beFalse
      }
    }
    "must have more specs" in {
      todo
    }
  }

}