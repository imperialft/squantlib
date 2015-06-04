package net.squantlib.model.asset

import net.squantlib.util.DisplayUtils._
import org.jquantlib.currencies.Currency
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.{Period => qlPeriod}
import net.squantlib.model.market.Market
import net.squantlib.util.UnderlyingParsers
import net.squantlib.database.DB
import net.squantlib.util.Date
import net.squantlib.util.initializer.Calendars

/**
 * Underlying to be used for pricing models.
 */

trait Underlying extends StaticAsset {
  
  val valuedate:Date
  
  val id:String
  
  lazy val assetName = id
  
  var spot:Double  // TO BE DEFINED IN SUBCLASS
  
  val currency:Currency // TO BE DEFINED IN SUBCLASS

  override lazy val fixingCalendar = Calendars(currency.code).getOrElse(Calendars.empty)

  override lazy val paymentCalendar = Calendars(currency.code).getOrElse(Calendars.empty)
  
  override def isPriced:Boolean = true
    
  override def latestPriceDate:Option[Date] = Some(valuedate)
    
  override def latestPriceLocalCcy:Option[Double] = Some(spot)
      
  val daysPerYr = 365.25
  
  
  /**
   * Returns the discounting rate (risk-free rate on the currency)
   * @param days observation date as the number of calendar days after value date.
   */
  def discountRate(days:Double):Double // TO BE DEFINED IN SUBCLASS
  def discountRate(date:Date):Double = discountRate(toDays(date))
  def discountRate(period:qlPeriod):Double = discountRate(toDays(period))
  def discountRate(dayfrac:Double, dayCounter:DayCounter):Double = discountRate(toDays(dayfrac, dayCounter))
  def discountRateY(years:Double):Double = discountRate(years * daysPerYr)
  
  def fwdDiscountRate(valuedate:Double, maturity:Double):Double = (discountRate(maturity) * maturity - discountRate(valuedate) * valuedate) / (maturity - valuedate)
  def fwdDiscountRate(valuedate:Date, maturity:Date):Double = fwdDiscountRate(toDays(valuedate), toDays(maturity))
  def fwdDiscountRate(valuedate:qlPeriod, maturity:qlPeriod):Double = fwdDiscountRate(toDays(valuedate), toDays(valuedate) + toDays(maturity))
  def fwdDiscountRateY(valuedateY:Double, maturityY:Double) = fwdDiscountRate(valuedateY * daysPerYr, maturityY * daysPerYr)
  
  /**
   * Returns the dividend yield mapped as #days from valuedate to dividend rate.
   * @param days observation date as the number of calendar days after value date.
   */
  def assetYield(days:Double):Double // TO BE DEFINED IN SUBCLASS
  def assetYield(date:Date):Double = assetYield(toDays(date))
  def assetYield(period:qlPeriod):Double = assetYield(toDays(period))
  def assetYield(dayfrac:Double, dayCounter:DayCounter):Double = assetYield(toDays(dayfrac, dayCounter))
  def assetYieldY(years:Double):Double = assetYield(years * daysPerYr)
  
  /**
   * Returns the repo yield mapped as #days from valuedate to repo rate.
   * @param days observation date as the number of calendar days after value date.
   */
  def repoRate(days:Double):Double // TO BE DEFINED IN SUBCLASS
  def repoRate(date:Date):Double = repoRate(toDays(date))
  def repoRate(period:qlPeriod):Double = repoRate(toDays(period))
  def repoRate(dayfrac:Double, dayCounter:DayCounter):Double = repoRate(toDays(dayfrac, dayCounter))
  def repoRateY(years:Double) = repoRate(years * daysPerYr)
    
  def fwdRepoRate(valuedate:Double, maturity:Double):Double = (repoRate(maturity) * maturity - repoRate(valuedate) * valuedate) / (maturity - valuedate)
  def fwdRepoRate(valuedate:Date, maturity:Date):Double = fwdRepoRate(toDays(valuedate), toDays(maturity))
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
  def impliedCorrelation(a:Underlying):Option[Double] = genericHistoricalCorrel(a)
  
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
  def volatility(date:Date):Double = volatility(toDays(date))  
  def volatility(date:Date, strike:Double):Double = volatility(toDays(date), strike)  
  
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
  def forward(date:Date):Double = forward(toDays(date))
    
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
  def forwardVol(valuedate:Date, maturity:Date):Double = 
    math.sqrt((toDays(maturity) * math.pow(volatility(maturity), 2.0) - toDays(valuedate) * math.pow(volatility(valuedate), 2.0)) / (toDays(maturity) - toDays(valuedate)))    
      
  /**
   * Returns forward volatility with strike
   * @param observation date as the nb year from value date.
   */
  def forwardVol(valuedate:Date, maturity:Date, strike:Double):Double = 
    math.sqrt((toDays(maturity) * math.pow(volatility(maturity, strike), 2.0) - toDays(valuedate) * math.pow(volatility(valuedate, strike), 2.0)) / (toDays(maturity) - toDays(valuedate))) 
    
  /**
   * Returns the latest defined date.
   */
  def maxDays:Double
    
  
  /**
   * Private date conversion functions
   */
  protected def toDays(dayfrac:Double, dayCounter:DayCounter) = (dayfrac * daysPerYr / dayCounter.annualDayCount)
  protected def toDays(date:Date) = (date.serialNumber - valuedate.serialNumber).toDouble
  protected def toDays(period:qlPeriod) = valuedate.days(period).toDouble
    
  override def getPriceHistory = DB.getHistorical(id)
    
  override protected def getDbForwardPrice = DB.getForwardPrices(assetID, id)
  
  def show(vd:List[Date]):Unit = {
    standardOutput("id:", id)
    standardOutput("valuedate", valuedate)
    standardOutput("spot", spot.asDouble)
    vd.foreach(d => standardOutput(d.toString, forward(d).asDouble, volatility(d).asPercent(2)))
  }
  
  def defaultShowPeriods = List("3M", "6M", "1Y", "2Y", "3Y", "5Y", "10Y", "20Y", "30Y").map(p => 
    valuedate.add(new qlPeriod(p))
    )
    
  def show:Unit = show(defaultShowPeriods)
    
} 


object Underlying {
  
  def apply(param:String, market:Market):Option[Underlying] = UnderlyingParsers.get(param).flatMap{case b => b.getUnderlying(market)}
  
}

