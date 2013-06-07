package squantlib.model

import squantlib.database.fixings.Fixings
import squantlib.database.DB
import squantlib.model.rates.DiscountCurve
import squantlib.util.initializer.Currencies
import squantlib.util.DisplayUtils._
import org.jquantlib.currencies.Currency
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod}
import java.util.{Date => JavaDate}

/**
 * Underlying to be used for pricing models.
 */
trait Underlying extends StaticAsset {
  
	val valuedate:qlDate
	
	val id:String
	
	var spot:Double  // TO BE DEFINED IN SUBCLASS
	
	/**
	 * Returns the atm implied volatility corresponding to the given date.
	 * @param days observation date as the number of calendar days after value date.
	 */
	def volatility(days:Double):Double // TO BE DEFINED IN SUBCLASS
	
	/**
	 * Returns the implied volatility corresponding to the given date & strike.
	 * @param days observation date as the number of calendar days after value date.
	 * @param strike 
	 */
	def volatility(days:Double, strike:Double):Double // TO BE DEFINED IN SUBCLASS
	
	/**
	 * Returns the implied volatility corresponding to the given date & strike.
	 * @param observation date as day count fraction and its day count method.
	 * @param strike
	 */
	def volatility(dayfrac:Double, dayCounter:DayCounter):Double = volatility(toDays(dayfrac, dayCounter))	
	def volatility(dayfrac:Double, dayCounter:DayCounter, strike:Double):Double = volatility(toDays(dayfrac, dayCounter), strike)	
	
	/**
	 * Returns the implied volatility corresponding to the given date & strike.
	 * @param observation date
	 * @param strike
	 */
	def volatility(date:qlDate):Double = volatility(toDays(date))	
	def volatility(date:qlDate, strike:Double):Double = volatility(toDays(date), strike)	
	
	/**
	 * Returns the implied volatility corresponding to the given date & strike.
	 * @param observation date
	 * @param observation date as the period from value date.
	 */
	def volatility(period:qlPeriod):Double = volatility(toDays(period))	
	def volatility(period:qlPeriod, strike:Double):Double = volatility(toDays(period), strike)	
	
	/**
	 * Returns the implied volatility corresponding to the given date represented in nb of years & strike.
	 * @param observation date
	 * @param observation date as the period from value date.
	 */
	def volatilityY(years:Double):Double = volatility(years * 365.25)
	def volatilityY(years:Double, strike:Double):Double = volatility(years * 365.25, strike)
	  
	/**
	 * Returns the forward price corresponding to the given date.
	 * @param observation date as the number of calendar days after value date.
	 */
    def forward(days:Double):Double // TO BE DEFINED IN SUBCLASS
    
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as day count fraction and its day count method.
	 */
    def forward(dayfrac:Double, dayCounter:DayCounter):Double = forward(toDays(dayfrac, dayCounter))
    
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date
	 */
    def forward(date:qlDate):Double = forward(toDays(date))
    
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the period from value date.
	 */
    def forward(period:qlPeriod):Double = forward(toDays(period))
    
	/**
	 * Returns the value corresponding to the given year.
	 * @param observation date as the nb year from value date.
	 */
    def forwardY(years:Double):Double = forward(years * 365.25)
    
	/**
	 * Returns forward atm volatility
	 * @param observation date as the nb year from value date.
	 */
    def forwardVol(valuedate:Double, maturity:Double):Double = 
      math.sqrt((maturity * math.pow(volatility(maturity), 2.0) - valuedate * math.pow(volatility(valuedate), 2.0)) / (maturity - valuedate))    
      
	/**
	 * Returns forward volatility with strike
	 * @param observation date as the nb year from value date.
	 */
    def forwardVol(valuedate:Double, maturity:Double, strike:Double):Double = 
      math.sqrt((maturity * math.pow(volatility(maturity, strike), 2.0) - valuedate * math.pow(volatility(valuedate, strike), 2.0)) / (maturity - valuedate))    
      
	/**
	 * Returns forward atm volatility
	 * @param observation date as the nb year from value date.
	 */
    def forwardVol(valuedate:qlDate, maturity:qlDate):Double = 
      math.sqrt((toDays(maturity) * math.pow(volatility(maturity), 2.0) - toDays(valuedate) * math.pow(volatility(valuedate), 2.0)) / (toDays(maturity) - toDays(valuedate)))    
      
	/**
	 * Returns forward volatility with strike
	 * @param observation date as the nb year from value date.
	 */
    def forwardVol(valuedate:qlDate, maturity:qlDate, strike:Double):Double = 
      math.sqrt((toDays(maturity) * math.pow(volatility(maturity, strike), 2.0) - toDays(valuedate) * math.pow(volatility(valuedate, strike), 2.0)) / (toDays(maturity) - toDays(valuedate))) 
    
	/**
	 * Returns the latest defined date.
	 */
    def maxDays:Double
    
	
	/**
	 * Private date conversion functions
	 */
    protected def toDays(dayfrac:Double, dayCounter:DayCounter) = (dayfrac * 365.25 / dayCounter.annualDayCount)
    protected def toDays(date:qlDate) = (date.serialNumber() - valuedate.serialNumber()).toDouble
    protected def toDays(period:qlPeriod) = period.days(valuedate).toDouble
    
    override def getPriceHistory = Fixings.getHistorical(id).getOrElse(Map.empty)
    
    override protected def getDbForwardPrice:Map[qlDate, Double] = DB.getForwardPricesTimeSeries(assetID, id).map{case (k, v) => (new qlDate(k), v)}
	
	def show(vd:List[qlDate]):Unit = {
	  println("id:\t" + id)
	  println("valuedate:\t" + valuedate)
	  println("spot:\t" + spot.asDouble)
	  vd.foreach(d => println("%tY/%<tm/%<td".format(d.longDate) + "\t" + forward(d).asDouble + "\t" + volatility(d).asPercent(2)))
	}
	
	def defaultShowPeriods = List("3M", "6M", "1Y", "2Y", "3Y", "5Y", "10Y", "20Y", "30Y").map(p => 
	  valuedate.add(new qlPeriod(p))
	  )
	  
	def show:Unit = show(defaultShowPeriods)
    
} 


object Underlying {
  
	def apply(param:String, market:Market) = getUnderlying(param, market)
  
	def getUnderlying(param:String, market:Market):Option[Underlying] = {
	  if (param == null) None
	  else param.trim match {
	    case "NKY" => market.getIndex("NKY")
	    case p if p.head.isDigit => market.getEquity(p)
	    case p => market.getFX(p)
	    }
	}
  
}

