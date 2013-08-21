package squantlib.model

import squantlib.database.DB
import squantlib.util.UnderlyingParser
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
  
  val daysPerYr = 365.25
  
  /**
   * Returns the discounting rate (risk-free rate on the currency)
   * @param days observation date as the number of calendar days after value date.
   */
  def discountRate(days:Double):Double // TO BE DEFINED IN SUBCLASS
  def discountRate(date:qlDate):Double = discountRate(toDays(date))
  def discountRate(period:qlPeriod):Double = discountRate(toDays(period))
  def discountRate(dayfrac:Double, dayCounter:DayCounter):Double = discountRate(toDays(dayfrac, dayCounter))
  def discountRateY(years:Double):Double = discountRate(years * daysPerYr)
  
  def fwdDiscountRate(valuedate:Double, maturity:Double):Double = (discountRate(maturity) * maturity - discountRate(valuedate) * valuedate) / (maturity - valuedate)
  def fwdDiscountRate(valuedate:qlDate, maturity:qlDate):Double = fwdDiscountRate(toDays(valuedate), toDays(maturity))
  def fwdDiscountRate(valuedate:qlPeriod, maturity:qlPeriod):Double = fwdDiscountRate(toDays(valuedate), toDays(valuedate) + toDays(maturity))
  def fwdDiscountRateY(valuedateY:Double, maturityY:Double) = fwdDiscountRate(valuedateY * daysPerYr, maturityY * daysPerYr)
  
  /**
   * Returns the dividend yield mapped as #days from valuedate to dividend rate.
   * @param days observation date as the number of calendar days after value date.
   */
  def assetYield(days:Double):Double // TO BE DEFINED IN SUBCLASS
  def assetYield(date:qlDate):Double = assetYield(toDays(date))
  def assetYield(period:qlPeriod):Double = assetYield(toDays(period))
  def assetYield(dayfrac:Double, dayCounter:DayCounter):Double = assetYield(toDays(dayfrac, dayCounter))
  def assetYieldY(years:Double):Double = assetYield(years * daysPerYr)
  
  /**
   * Returns the repo yield mapped as #days from valuedate to repo rate.
   * @param days observation date as the number of calendar days after value date.
   */
  def repoRate(days:Double):Double // TO BE DEFINED IN SUBCLASS
  def repoRate(date:qlDate):Double = repoRate(toDays(date))
  def repoRate(period:qlPeriod):Double = repoRate(toDays(period))
  def repoRate(dayfrac:Double, dayCounter:DayCounter):Double = repoRate(toDays(dayfrac, dayCounter))
  def repoRateY(years:Double) = repoRate(years * daysPerYr)
    
  def fwdRepoRate(valuedate:Double, maturity:Double):Double = (repoRate(maturity) * maturity - repoRate(valuedate) * valuedate) / (maturity - valuedate)
  def fwdRepoRate(valuedate:qlDate, maturity:qlDate):Double = fwdRepoRate(toDays(valuedate), toDays(maturity))
  def fwdRepoRate(valuedate:qlPeriod, maturity:qlPeriod):Double = fwdRepoRate(toDays(valuedate), toDays(valuedate) + toDays(maturity))
  def fwdRepoRateY(valuedateY:Double, maturityY:Double) = fwdRepoRate(valuedateY * daysPerYr, maturityY * daysPerYr)
  
  /**
   * Returns the dividends mapped as #days from valuedate to amount of dividend
   * @param days observation date as the number of calendar days after value date.
   */
  val dividends:Map[Double, Double]  // TO BE DEFINED IN SUBCLASS
  def dividendList:List[(Double, Double)] = dividends.toList.sortBy(_._1)
  def dividendsY:Map[Double, Double] = dividends.map{case (d, v) => (d / daysPerYr, v)}
  def dividendListY:List[(Double, Double)] = dividendList.map{case (d, v) => (d / daysPerYr, v)}
    
  /**
   * Returns the implied correlation against another underlying. Default is daily observation for 1yr (260 biz dates) period.
   * @param another underlying
   */
  def impliedCorrelation(a:Underlying):Option[Double] = historicalCorrelLatestValue(a, 260)
  
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
  def volatilityY(years:Double):Double = volatility(years * daysPerYr)
  def volatilityY(years:Double, strike:Double):Double = volatility(years * daysPerYr, strike)
    
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
  def forwardY(years:Double):Double = forward(years * daysPerYr)
    
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
  protected def toDays(dayfrac:Double, dayCounter:DayCounter) = (dayfrac * daysPerYr / dayCounter.annualDayCount)
  protected def toDays(date:qlDate) = (date.serialNumber() - valuedate.serialNumber()).toDouble
  protected def toDays(period:qlPeriod) = period.days(valuedate).toDouble
    
  override def getPriceHistory = DB.getHistorical(id)
    
  override protected def getDbForwardPrice:Map[qlDate, Double] = DB.getForwardPrices(assetID, id)
  
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
  
  def getUnderlying(param:String, market:Market):Option[Underlying] = UnderlyingParser.getUnderlying(param, market)
  
}

