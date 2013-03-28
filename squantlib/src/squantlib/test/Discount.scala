package squantlib.test

import scala.math._
import scala.annotation.tailrec

object Discount {
 
  //value types
  val integer:Int = 12
  val longinteger:Long = 12
  val double:Double = 12.27 // don't use equal use a - b < 1e-10 
  val deci:BigDecimal = 12.27 // can use equal 
  
  // basic object types
  val str:String = "test"
  
  //collection
  val list:List[Double] = List(2.2, 3.3, 4.4, 5.5) // ordered, points to head
  val array:Array[Double] = Array(2.2, 3.3, 4.4, 5.5) // ordered
  val set:Set[Double] = Set(2.2, 2.2, 3.3, 4.4, 5.5) // not ordered, unique
  val map:Map[String, Double] = Map("z" -> 1.1, "a" -> 2.2, "c" -> 1.5, "a" -> 5) // unique key, "hashmap" / "dictionary"
 
  // collection functions
  val list2:List[Double] = list.map(v => v * 2) // Map returns collection
  val list3:List[Double] = list.map(v => {
    val w = v + 1
    w * 2
  })
  
//  list.foreach(v => println(v * 2)) // Foreach does not return collection
 
  val forresult = for(i <- 1 to 3) yield {val j = i + 1; j * 3} // not ordered
 
  def recFunction(d:Int):Int = if (d <= 0) d else d + recFunction(d-1)
 

 
  // returns discount factor for libor with Xyr maturity
  def dfOnePayment(maturity:Int):Double = {
    if (maturity <= 1) 1.0 / (SwapJPY(maturity)  + 1.0)  else Double.NaN
  }
  // returns discount factor for 2yr maturity with annual payment
  def dfTwoYear:Double = {
    val dfOneY:Double = {dfOnePayment(1)* SwapJPY(1) }
    (1.0 - dfOneY / (SwapJPY(2)  + 1.0) )  }

  def dfNYear(maturity:Int, period:Int):Double = dfNYear(maturity, SwapJPY, period)
 
  // returns discount factor for Nyr maturity with annual payment
  def dfNYear(maturity:Int, rates:Map[Double, Double], period:Int):Double = {
    if(maturity == 1) dfOnePayment(1)
    else if (maturity >= 11) Double.NaN
    else {
      val notional = 1.0
      val daycount = 1.0 / period.toDouble
     val sumzc = (for (i <- 1 to maturity - 1) yield dfNYear(i, rates, period)).sum
      (notional - rates(maturity) * daycount * sumzc) / (1.0 + rates(maturity) * daycount)
    }
  }
  
  def priceCheck(maturity:Int, period:Int):Double = priceCheck(maturity:Int, SwapJPY, period)
 
  def priceCheck(maturity:Int, rates:Map[Double, Double], period:Int):Double = {
    val rate = rates(maturity)
    val coupons = for (i <- 1 to maturity) yield rate * dfNYear(i, rates, period)
    val redemption = dfNYear(maturity, rates, period)
   
    coupons.foreach(c => println(c))
    println(redemption)
   
    coupons.sum + redemption
  }
 
// def dfDblYear(m:Double,p:Double):Double = dfDblYear(m, ratesSemiannual,p)
////****** returns discount factor for Nyr maturity with annual payment
// def dfDblYear(m:Double, rates:Map[Double, Double],p:Double):Double = {
//    		val notional = 1.0
//    		if(m <= 1.0) notional / (ratesSemiannual(1)*p  + notional)
//    			else if (m >= 11.0) Double.NaN
//    			else { 
//    				  val sumzc2 = (for (i2 <- 1 to m.toInt - 1) yield dfDblYear(i2, rates, p)).sum
//    				(notional - rates(m) * sumzc2) / (1.0 + rates(m) * p)}   		
//  }
 
//****    for (i <- 1 to 10) println(i.toString + ": " + dfNYear(i, 1)) 

// returns discount factor for Nyr maturity with semiannual payment
 def dfNYearSemiannual(m1:Double, p1:Int):Double = dfNYearSemiannual(m1, SwapJPY, p1)
 
 def dfNYearSemiannual(m1:Double, rates:Map[Double, Double], p1:Int):Double = {
  val notional = 1.0
  val daycount:Double =  p1.toDouble / 12.0
  if(m1 <= daycount) notional / (SwapJPY(m1)*daycount + notional)
    else if (m1 >= 11.0 ) Double.NaN
    else {
      val m2 = (m1 / daycount)toInt
      val zc2 =  for (i <- 1 to m2 - 1) yield  dfNYearSemiannual((i.toDouble * daycount), rates, p1)
      zc2.foreach(println)
      val sumzc2 = zc2.sum
       (notional -  rates(m1) * daycount * sumzc2) / (1.0 + rates(m1) * daycount)
    }
  }
 
 def priceCheck1(m1:Double, p1:Int):Double = priceCheck1(m1, SwapJPY, p1)
 
 def priceCheck1(m1:Double, rates:Map[Double, Double], p1:Int):Double = {
    val rate = rates(m1)
    val daycount:Double =  p1.toDouble / 12.0
    val m2 = (m1 / daycount)toInt
    val coupons =  for (i <- 1 to m2 - 1) yield  rate*dfNYearSemiannual((i.toDouble * daycount), rates, p1)*daycount
    val redemption = dfNYearSemiannual(m1, rates, p1)
   
    coupons.foreach(c => println(c))
    println(redemption)
   
     coupons.sum +  (1.0 + (rate * daycount))*redemption
  }

 //課題１　periodが12でも６でも３でも動くように
 //maturityがどんな数でもratesSemiannualから線形保管してrateを出す
 
 def rate(m:Double, curve:Map[Double, Double]):Double = { 
   val xs = curve.keySet
   val minrange = xs.min
   val maxrange = xs.max
   
   m match {
     case v if v <= minrange => curve(minrange)
     case v if v >= maxrange => curve(maxrange)
     case v if xs contains v => curve(v)
     case v => 
       val xlow = xs.filter(x => x < v).max
       val xhigh = xs.filter(x => x > v).min
       curve(xlow) + (curve(xhigh) - curve(xlow)) * (v - xlow) / (xhigh - xlow)
   }
  }
 
 def JPYrate(m:Double) = rate(m, SwapJPY)
 def USDYrate(m:Double) = rate(m, SwapUSD)
 
 //maturityとperiodに応じてrate Mapを作る
 // maturity in nb years, period in months
 //LIBOR+aのとき
 // returns map maturity -> swap rate
 def rateMPA(maturity:Double, period:Int, spread:Double):Map[Double, Double] = {
   val daycount:Double = period.toDouble / 12.0
   val nbperiods = (maturity / daycount).toInt
   val fullperiods = for (i <- 1 to nbperiods-1) yield (i * daycount, JPYrate(i * daycount) + spread)
   val broken = (maturity, JPYrate(maturity) + spread)
   (fullperiods :+ broken).toMap
 }
 
 //rateMPをつかう（今までのratesSemiannualのかわり）前提としてmaturityはperiodで割り切れる
 def dfNYearNperiod(m1:Double, p2:Int):Double = dfNYearNperiod(m1, rateMPA(m1,p2, 0.0), p2)
 
 def dfNYearNperiod(m1:Double, rates:Map[Double, Double], p2:Int):Double = {
   val notional = 1.0
   val daycount:Double =  p2.toDouble / 12.0
   if (m1 < daycount) Double.NaN
   else if (m1 == daycount) notional / (rates(daycount) * daycount + notional)
   else if (m1 >= 11.0 ) Double.NaN
   else {
     val zc2 = for (i <- 1 to (m1 /daycount).toInt-1) yield  dfNYearNperiod((i.toDouble * daycount), rates, p2)
     val sumzc2 = zc2.sum
     (notional -  rates(m1) * daycount * sumzc2) / (notional + rates(m1) * daycount)
  }
 }

//課題２　上のことが確かめられるファンクション作る
 def priceCheckNYNP(m1:Double, p2:Int):Double = priceCheckNYNP(m1, rateMPA(m1,p2, 0.0), p2)
 
 def priceCheckNYNP(m1:Double, rates:Map[Double, Double], p2:Int):Double = {
    val rate = rates(m1)
    val daycount:Double =  p2.toDouble / 12.0
    val coupons =  for (i <- 1 to (m1 /daycount).toInt-1) yield  rate*dfNYearNperiod((i.toDouble * daycount), rates, p2)*daycount
    val redemption = dfNYearNperiod(m1, rates, p2)
   
    coupons.foreach(c => println(c))
    println(redemption)
   
     coupons.sum +  (1.0 + (rate * daycount))*redemption
  }
 
//課題２．５　maturityがperiodで割り切れないとき　longを後ろに付ける
  def dfFreeMNperiod(maturity:Double, period:Int, spread:Double = 0.0):Double = dfFreeMNperiod(maturity, rateMPA(maturity, period, spread), period)
  
  def dfFreeMNperiod(maturity:Double, rates:Map[Double, Double], period:Int):Double = 
    if (rates.isEmpty) Double.NaN
    else dfFreeMNperiod(rates.toList.sortBy(_._1), 0.0, 0.0)
 
  /*
   * compute discount factor from given date-rate set.
   * input parameter must be sorted by date.
   */
  @tailrec def dfFreeMNperiod(rates:List[(Double, Double)], currentPeriod:Double, currentSum:Double):Double = {
    val (period, rate) = rates.head
    val daycount = period - currentPeriod
    val zc = (1.00 -  rate * currentSum) / (1.00 + rate * daycount)
//    println("period: " + period + " rate:" + rate + " daycount:" + daycount + " zc:" + zc)
    
    if (rates.size == 1) zc
    else dfFreeMNperiod(rates.tail, period, currentSum + zc * daycount)
  }
  
  
  /*
   * compute price of fixed rate bond with swap rate + spread as fixed coupon.
   */
  def priceJPYBond(maturity:Double, period:Int, spread:Double):Double = {
    val paydates = rateMPA(maturity, period, 0.0).keySet.toList.sorted
    val daycounts = paydates.head +: (for (i <- 1 to paydates.size-1) yield paydates(i) - paydates(i-1))
    val fixedrate = JPYrate(maturity)
    val schedule:List[(Double, Double)] = (for(i <- 0 to paydates.size - 1) yield (paydates(i), fixedrate * daycounts(i))).toList :+ (maturity, 1.0)
    
    val legprice = schedule.map{case (date, amount) => 
      val zc = dfFreeMNperiod(date, period, spread)
      println("date:" + date + " amount:" + amount + " zc:" + zc + " price:" + (amount * zc))
      amount * zc}
    
    legprice.sum
  }
  
 
 def priceCheckFMNP(maturity:Double, period:Int):Double = priceCheckFMNP(maturity, rateMPA(maturity, period, 0.0), period)
 
 def priceCheckFMNP(maturity:Double, rates:Map[Double, Double], period:Int):Double = {
   val daycount:Double =  period.toDouble / 12.0
   if (maturity < daycount) Double.NaN 
   else{
	val coupons =  for (i <- 1 to (maturity /daycount).toInt-1) yield  JPYrate(maturity)*dfFreeMNperiod((i.toDouble * daycount), rates, period)*daycount
	val redemption = dfFreeMNperiod(maturity, rates, period)
    val md = maturity / daycount
    coupons.foreach(c => println("coupon " + c))
    println(redemption)
    		if(md - md.toInt == 0){ coupons.sum +  (1.0 + (JPYrate(maturity)* daycount))*redemption}
    		else {val mm = maturity - ((maturity /daycount).toInt*daycount)
    				coupons.sum +  (1.0 + (JPYrate(maturity)*mm))*redemption}
   }
  }

//課題３　発行者の信用リスクがゼロじゃなくてαの場合の式はどうなるか」を考えておいてください。
//LIBORで貸し出せるのではなく、LIBOR+α%で貸し出せるということです。
 def dfFMNPplusa(m1:Double, p2:Int, a:Double):Double = dfFMNPplusa(m1, rateMPA(m1,p2,a), p2,a)
 
 def dfFMNPplusa(m1:Double, rates:Map[Double, Double], p2:Int,a:Double):Double = {
    	val notional = 1.0
    	val daycount:Double =  p2.toDouble / 12.0
    	val md = m1 / daycount
    	if (md - md.toInt == 0){
    							if(m1 < daycount)Double.NaN
    							else if(m1 == daycount) notional / (rates(daycount) * daycount + notional)
    							else if (m1 >= 11.0 ) Double.NaN
    							else {
    								val zc2 =  for (i <- 1 to (m1 /daycount).toInt-1) yield  dfFMNPplusa((i.toDouble * daycount), rates, p2,a)
    								//zc2.foreach(println)
    								val sumzc2 = zc2.sum
    								(notional -  rates(m1) * daycount * sumzc2) / (notional + rates(m1) * daycount)
    							}}
    	else {  if(m1 < daycount) Double.NaN
    			else if (m1 >= 11.0 ) Double.NaN
    			else {	val zc2 =  for (i <- 1 to (m1 /daycount).toInt-1) yield  dfFMNPplusa((i.toDouble * daycount), rates, p2,a)
    					//zc2.foreach(println)
    					val sumzc2 = zc2.sum
    					val mm = m1 - ((m1 /daycount).toInt*daycount)
    					(notional -  (JPYrate(m1)+a) *daycount* sumzc2) / (notional + (JPYrate(m1)+a) *mm)
    							}
    	}
  }
 
 def priceCheckFMNPplusa(m1:Double, p2:Int, a:Double):Double = priceCheckFMNPplusa(m1, rateMPA(m1,p2,a), p2,a)
 
 def priceCheckFMNPplusa(m1:Double, rates:Map[Double, Double], p2:Int,a:Double):Double = {
   val daycount:Double =  p2.toDouble / 12.0
   if(m1 < daycount) Double.NaN 
   else{
	val coupons =  for (i <- 1 to (m1 /daycount).toInt-1) yield  (JPYrate(m1)+a)*dfFMNPplusa((i.toDouble * daycount), rates, p2,a)*daycount
	val redemption = dfFMNPplusa(m1, rates, p2,a)
    val md = m1 / daycount
    coupons.foreach(c => println("coupon " + c))
    println(redemption)
    		if(md - md.toInt == 0){ coupons.sum +  (1.0 + ((JPYrate(m1)+a)* daycount))*redemption}
    		else {val mm = m1 - ((m1 /daycount).toInt*daycount)
    				coupons.sum +  (1.0 + ((JPYrate(m1)+a)*mm))*redemption}
   }
  }
  
 
 // def hello(d:Double):Unit = println("Hello " + d.toString)
        
//Continuous Compounding Rate
 //log(x:Double):Double
  def dfCCR(m1:Double, p2:Int, a:Double):Double = {
   var x = dfFMNPplusa(m1, p2, a)
   -log(x:Double) / m1
    }
 
//********** Cross Currency Swap *********************************************************************************
//maturityがどんな数でもSwapUSDから線形保管してrateを出す
 def rateSUSD(m:Double):Double = { 
  val i = 1.0
  if (m < i)  SwapUSD(i)
  else {
	  	val md:Double = m - m.toInt
			  if (md == 0) SwapUSD(m)
			  else { val a:Double = md * (SwapUSD(m.toInt + 1) - SwapUSD(m.toInt))
    	            SwapUSD(m.toInt) + a }}
}
//LIBOR+aのとき
 def rateSwapUSDA(m:Double, p2:Int, a:Double):Map[Double, Double] = {
   val daycount:Double=  p2.toDouble / 12.0
  // List("A"->123, "B"->456).toMap Map((A,123), (B,456))
   ( (for(i <- 1 to (m /daycount).toInt) yield  ((i*daycount).toDouble , rateSUSD(i * daycount) + a) ).toMap
      )   
 }  
//maturityがperiodで割り切れないとき　longを後ろに付ける 
def dfCrossCurrencySwap(m1:Double, p2:Int, a:Double):Double = dfCrossCurrencySwap(m1, rateSwapUSDA(m1,p2,a), p2, a)
 
def dfCrossCurrencySwap(m1:Double, rates:Map[Double, Double], p2:Int, a:Double):Double = {
    	val notional = 1.0
    	val daycount:Double =  p2.toDouble / 12.0
    	val md = m1 / daycount
    	if (md - md.toInt == 0){
    							if(m1 < daycount)Double.NaN
    							else if(m1 == daycount) notional / (rates(daycount) * daycount + notional)
    							else if (m1 >= 11.0 ) Double.NaN
    							else {
    								val zc2 =  for (i <- 1 to (m1 /daycount).toInt-1) yield  dfCrossCurrencySwap((i.toDouble * daycount), rates, p2,a)
    								val sumzc2 = zc2.sum
    								(notional -  rates(m1) * daycount * sumzc2) / (notional + rates(m1) * daycount)
    							}}
    	else {  if(m1 < daycount) Double.NaN
    			else if (m1 >= 11.0 ) Double.NaN
    			else {	val zc2 =  for (i <- 1 to (m1 /daycount).toInt-1) yield dfCrossCurrencySwap((i.toDouble * daycount), rates, p2,a)
    					val sumzc2 = zc2.sum
    					val mm = m1 - ((m1 /daycount).toInt*daycount)
    					(notional -  (rateSUSD(m1)+a) *daycount* sumzc2) / (notional + (rateSUSD(m1)+a) *mm)
    							}
    	}
  }

def priceCheckCrossCurrencySwap(m1:Double, p2:Int, a:Double):Double = priceCheckCrossCurrencySwap(m1, rateSwapUSDA(m1,p2,a), p2,a)
 
def priceCheckCrossCurrencySwap(m1:Double, rates:Map[Double, Double], p2:Int,a:Double):Double = {
  val daycount:Double =  p2.toDouble / 12.0
   if(m1 < daycount) Double.NaN 
   else{
	val coupons =  for (i <- 1 to (m1 /daycount).toInt-1) yield  (rateSUSD(m1)+a)*dfCrossCurrencySwap((i.toDouble * daycount), rates, p2,a)*daycount
	val redemption1 = dfCrossCurrencySwap(m1, rates, p2, a)
    val md = m1 / daycount
    coupons.foreach(c => println("coupon " + c))
    println(redemption1)
    		if(md - md.toInt == 0){ coupons.sum +  (1.0 + ((rateSUSD(m1)+a)* daycount))*redemption1}
    		else {val mm = m1 - ((m1 /daycount).toInt*daycount)
    				coupons.sum +  (1.0 + ((rateSUSD(m1)+a)*mm))*redemption1}
   }
  }

/////CCS 円
//maturityがどんな数でもSwapUSDから線形保管してrateを出す
 def rateSY(m:Double):Double = { 
  val i = 1.0
  if (m < i)  SwapYen(i)
  else {
	  	val md:Double = m - m.toInt
			  if (md == 0) SwapYen(m)
			  else { val a:Double = md * (SwapYen(m.toInt + 1) - SwapYen(m.toInt))
    	            SwapYen(m.toInt) + a }}
}
 def SYB(m:Double):Double = { 
  val i = 1.0
  if (m < i)  BasisSwapJPY(i)
  else {
	  	val md:Double = m - m.toInt
			  if (md == 0) BasisSwapJPY(m)
			  else { val a:Double = md * (BasisSwapJPY(m.toInt + 1) - BasisSwapJPY(m.toInt))
    	            BasisSwapJPY(m.toInt) + a }}
}
//LIBOR+bのとき
 def rateSYB(m:Double, p2:Int):Map[Double, Double] = {
   val daycount:Double=  p2.toDouble / 12.0
  // List("A"->123, "B"->456).toMap Map((A,123), (B,456))
   ( (for(i <- 1 to (m /daycount).toInt) yield  ((i*daycount).toDouble , rateSY(i * daycount) + SYB(i * daycount)) ).toMap
      )   
 }  
//maturityがperiodで割り切れないとき　longを後ろに付ける 
def dfCCSYen(m1:Double, p2:Int):Double = dfCCSYen(m1, rateSYB(m1,p2), p2)
 
def dfCCSYen(m1:Double, rates:Map[Double, Double], p2:Int):Double = {
    	val notional = 1.0
    	val daycount:Double =  p2.toDouble / 12.0
    	val md = m1 / daycount
    	if (md - md.toInt == 0){
    							if(m1 < daycount)Double.NaN
    							else if(m1 == daycount) notional / (rates(daycount) * daycount + notional)
    							else if (m1 >= 11.0 ) Double.NaN
    							else {
    								val zc2 =  for (i <- 1 to (m1 /daycount).toInt-1) yield  dfCCSYen((i.toDouble * daycount), rates, p2)
    								val sumzc2 = zc2.sum
    								(notional -  rates(m1) * daycount * sumzc2) / (notional + rates(m1) * daycount)
    							}}
    	else {  if(m1 < daycount) Double.NaN
    			else if (m1 >= 11.0 ) Double.NaN
    			else {	val zc2 =  for (i <- 1 to (m1 /daycount).toInt-1) yield dfCCSYen((i.toDouble * daycount), rates, p2)
    					val sumzc2 = zc2.sum
    					val mm = m1 - ((m1 /daycount).toInt*daycount)
    					(notional -  (rateSY(m1) + SYB(m1) ) *daycount* sumzc2) / (notional + (rateSY(m1) + SYB(m1) ) *mm)
    							}
    	}
  }

def priceCCSYen(m1:Double, p2:Int):Double = priceCCCSYen(m1, rateSYB(m1,p2), p2)
 
def priceCCCSYen(m1:Double, rates:Map[Double, Double], p2:Int):Double = {
  val daycount:Double =  p2.toDouble / 12.0
   if(m1 < daycount) Double.NaN 
   else{
	val coupons =  for (i <- 1 to (m1 /daycount).toInt-1) yield  (rateSY(m1)+ SYB(m1) )*dfCCSYen((i.toDouble * daycount), rates, p2)*daycount
	val redemption1 = dfCCSYen(m1, rates, p2)
    val md = m1 / daycount
    coupons.foreach(c => println("coupon " + c))
    println(redemption1)
    		if(md - md.toInt == 0){ coupons.sum +  (1.0 + ((rateSY(m1) + SYB(m1) )* daycount))*redemption1}
    		else {val mm = m1 - ((m1 /daycount).toInt*daycount)
    				coupons.sum +  (1.0 + ((rateSY(m1)+ SYB(m1) )*mm))*redemption1}
   }
  }


def CCSwap(m1:Double, p2:Int, a:Double):Double = {
  val DfUSD = dfCrossCurrencySwap(m1,p2,a)
  println("USD Df = " +DfUSD)
  dfCCSYen(m1,p2)
  ///////円のファンクションの結果を出す
}

val SwapJPY = Set(
    (0.5, 0.029661),
(1.0, 0.029661),
(1.5, 0.030051890027484707),
(2.0, 0.03057),
(2.5, 0.031289937274582284),
(3.0, 0.03207585885629128),
(3.5, 0.0327832586302965),
(4.0, 0.0334463421163121),
(4.5, 0.03413084213359062),
(5.0, 0.03484600622729594),
(5.5, 0.035575769552710704),
(6.0, 0.03625951271964639),
(6.5, 0.03684919945087),
(7.0, 0.037394003834913835),
(7.5, 0.03794933328177461),
(8.0, 0.03849423724849485),
(8.5, 0.03899483395929381),
(9.0, 0.039445335049155685),
(9.5, 0.03985113326996372),
(10.0, 0.04023383810198704)).toMap
             
val SwapUSD = Set(
		(1.0, 0.003485),
		(2.0, 0.004165),
		(3.0, 0.005355),
		(4.0, 0.00727),
		(5.0, 0.00971),
		(6.0, 0.01232),
		(7.0, 0.01473),
		(8.0, 0.0169),
		(9.0, 0.018795),
		(10.0, 0.02047)).toMap
val SwapYen = Set(
    (10.0,	0.007017),
	(9.0,	0.006075),
(8.0,	0.005125),
(7.0,	0.004275),
(6.0,	0.0035),
(5.0,	0.0028835),
(4.0,	0.00253),
(3.0,	0.002325),
(2.0,	0.0022505),
(1.0,	0.0023398)
).toMap

val BasisSwapJPY = Set(
(10.0,	-0.00541),
(9.0,	-0.00554),
(8.0,	-0.00566),
(7.0,	-0.00578),
(6.0,	-0.00565),
(5.0,	-0.00553),
(4.0,	-0.00508),
(3.0,	-0.00438),
(2.0,	-0.00343),
(1.0,	-0.00214)
		).toMap
		
		
}
