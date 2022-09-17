package net.squantlib.util

import scala.annotation.tailrec
import DisplayUtils._
import net.squantlib.util.initializer._
import net.squantlib.database.DB
import net.squantlib.util.RoundingInfo

case class FixingInformation(
  currencyId:String,
  paymentCurrencyId: String,
  var tbd:Option[BigDecimal],
  var minRange:Option[BigDecimal],
  var maxRange:Option[BigDecimal],
  fixingPageInformation: List[Map[String, String]],
  paymentCalendar: DbCalendar,
  isInitial:Boolean = false,
  isStrike:Boolean = false,
  initialUnderlyingFixings:Option[UnderlyingFixing] = None,
) {

  lazy val noFallbackUnderlyingIds:List[String] = fixingPageInformation.filter(pageInfo => pageInfo.get("no_fallback") == Some("1")).map(pageInfo => pageInfo.get("underlying")).flatMap(s => s)

  def isFallbackToSystem(underlyingId:String) = !noFallbackUnderlyingIds.contains(underlyingId)

  lazy val initialFixingInformation:Option[FixingInformation] = {
    if (isInitial) None
    else {
      var newFixingPageInfo = fixingPageInformation.map(pageInfo => {
        var newPageInfo = pageInfo
        if (pageInfo.contains("initial_rounding")) {
          newPageInfo = newPageInfo.updated("rounding", newPageInfo("initial_rounding"))
        }
        if (pageInfo.contains("initial_precision")) {
          newPageInfo = newPageInfo.updated("precision", newPageInfo("initial_precision"))
        }
        newPageInfo
      })
  
      Some(
        FixingInformation(
          currencyId,
          paymentCurrencyId,
          tbd,
          minRange,
          maxRange,
          newFixingPageInfo,
          paymentCalendar,
          true,
          isStrike,
          Some(initialFixing)
        )
      )
    }
 }

  lazy val strikeFixingInformation:Option[FixingInformation] = {
    if (isStrike) None
    else {
      var newFixingPageInfo = fixingPageInformation.map(pageInfo => {
        var newPageInfo = pageInfo
        if (pageInfo.contains("strike_rounding")) {
          newPageInfo = newPageInfo.updated("rounding", newPageInfo("strike_rounding"))
        }
        if (pageInfo.contains("strike_precision")) {
          newPageInfo = newPageInfo.updated("precision", newPageInfo("strike_precision"))
        }
        newPageInfo
      })

      Some(
        FixingInformation(
          currencyId,
          paymentCurrencyId,
          tbd,
          minRange,
          maxRange,
          newFixingPageInfo,
          paymentCalendar,
          isInitial,
          true,
          Some(initialFixing)
        )
      )
    }
  }

  lazy val partialAssetRounding:Map[String, RoundingInfo] = {
    fixingPageInformation.map(pageInfo => {
      val underlyingId = pageInfo.get("underlying")
      val rounding = pageInfo.get("partial_asset_rounding")
      val precision = pageInfo.get("partial_asset_precision").flatMap{case s => s.parseInt}

      (underlyingId, rounding, precision) match {
        case (Some(uid), Some(r), Some(p)) => Some((uid, new RoundingInfo(p, r)))
        case _ => None
      }
    }).collect{case Some(v) => v}.toMap
  }

  lazy val assetTradedUnit:Map[String, Int] = { 
    fixingPageInformation.map(pageInfo => {
      val underlyingId = pageInfo.get("underlying")
      val tradedUnit = pageInfo.get("traded_unit").flatMap{case s => s.parseInt}

      (underlyingId, tradedUnit) match {
        case (Some(uid), Some(r)) => Some((uid, r))
        case _ => None
      }
    }).collect{case Some(v) => v}.toMap
  }

  def getInitialFixingInformation:FixingInformation = initialFixingInformation.getOrElse(this)

  def getStrikeFixingInformation:FixingInformation = strikeFixingInformation.getOrElse(this)

  var initialFixing:UnderlyingFixing = initialUnderlyingFixings.getOrElse(UnderlyingFixing.empty)

  def setInitialFixingDouble(vs:Map[String, Double]) = {
    initialFixing = UnderlyingFixing(vs)(this)
  }

  def setInitialFixingDecimal(vs:Map[String, BigDecimal]) = {
    initialFixing = UnderlyingFixing(vs)
  }


  def initialFixingFull:UnderlyingFixing = {
    val inv:Map[String, Option[BigDecimal]] = initialFixing.getDecimalValue
      .withFilter{case (k, v) => k.size == 6}
      .map{case (k, v) => (((k takeRight 3) + (k take 3)), (if (v == 0.0) None else Some(1.0 / v)))}
      .collect{case (k, Some(v)) => (k, Some(v))}

    if (inv.isEmpty) initialFixing else UnderlyingFixing(initialFixing.getDecimal ++ inv)
  }
    
  def all:UnderlyingFixing = tbd match {
    case Some(c) => UnderlyingFixing(initialFixingFull.getDecimal.updated("tbd", Some(c)))
    case None => initialFixingFull
  }

  def update(p:String):String = {
    multipleReplace(p, all.getDecimalValue.map{case (k, v) => ("@" + k, v)})
  }
  
  def updateInitial(p:String):String = multipleReplace(p, initialFixing.getDecimalValue.map{case (k, v) => ("@" + k, v)})

  private def multipleReplace[T:Numeric](s:String, replacements:Map[String, T]):String = multipleReplace(s, replacements.toList.sortBy(-_._1.size))

  @tailrec private def multipleReplace[T:Numeric](s:String, replacements:List[(String, T)]):String =
    if (s == null) null
    else replacements match {
      case Nil => s
      case (k, v) :: xs => multipleReplace(s.replace(k, v.toString), xs)//replacements - k)
    }
  
  def updateCompute(p:String):Option[Double] = {
    FormulaParser.calculate(update(p))
  }
  
  /*
   * Fixing Information Accessor
   */
    
  def currentPercent(decimal:Int):String = tbd.collect{case v => v.asPercent(decimal)}.getOrElse("未定")
    
  def currentDouble(decimal:Int):String = tbd.collect{case v => v.asDouble(decimal)}.getOrElse("未定")
  
  def minRangePercent(decimal:Int):String = minRange.collect{case v => v.asPercent(decimal)}.getOrElse("未定")
    
  def minRangeDouble(decimal:Int):String = minRange.collect{case v => v.asDouble(decimal)}.getOrElse("未定")
    
  def maxRangePercent(decimal:Int):String = maxRange.collect{case v => v.asPercent(decimal)}.getOrElse("未定")
    
  def maxRangeDouble(decimal:Int):String = maxRange.collect{case v => v.asDouble(decimal)}.getOrElse("未定")
    
  def rangePercent(decimal:Int):String = (minRange, maxRange) match {
    case (min, max) if min.isDefined || max.isDefined => s"[${min.collect{case v => v.asPercent(decimal)}.getOrElse("")}～${max.collect{case v => v.asPercent(decimal)}.getOrElse("")}]"
    case _ => ""
  }
    
  def rangeDouble(decimal:Int):String = (minRange, maxRange) match {
    case (min, max) if min.isDefined || max.isDefined => s"[${min.collect{case v => v.asDouble(decimal)}.getOrElse("")}～${max.collect{case v => v.asDouble(decimal)}.getOrElse("")}]"
    case _ => ""
  }

  val underlyingFixingPage:Map[String, FixingPage] = fixingPageInformation.map(pageInfo => {
    val bidOffer:Option[String] = pageInfo.get("bidoffer") match {
      case Some(b) if b == "bid" || b == "offer" || b == "mid" => Some(b)
      case _ => None
    }

    val underlyingId = pageInfo.getOrElse("underlying", "missingUnderlying")

    val precision:Int = pageInfo.get("precision").collect{case s => s.toInt}.getOrElse(DB.getUnderlyingDefaultPrecision(underlyingId))

    val roundType:String = pageInfo.get("rounding").getOrElse("rounded")

    (
      underlyingId,
      FixingPage(
        underlying = underlyingId,
        page = pageInfo.get("page"), //getOrElse("page", "missingPage"),
        bidOffer = bidOffer,
        time = pageInfo.get("time"),
        country = pageInfo.get("country"),
        priceType = pageInfo.get("price_type").getOrElse("close"),
        initialPriceType = pageInfo.get("initial_price_type").getOrElse("close"),
        precision = precision,
        roundType = roundType,
        fallbackToSystem = isFallbackToSystem(underlyingId)
      )
    )
  }).toMap


  val underlyingPrecisions:Map[String, Int] = {
    val basePrecision = underlyingFixingPage.map{case (ul, fixingPage) => (ul, fixingPage.precision)}
    val inversePrecision = (underlyingFixingPage.keySet.filter(ul => Currencies.isForex(ul) && !underlyingFixingPage.contains(ul.takeRight(3) + ul.take(3)))).map(ul => (ul.takeRight(3) + ul.take(3), 25)).toMap
    inversePrecision ++ basePrecision
  }


  def getUnderlyingPrecision(underlyingId:String):Int = {
    val uid = {
      if (underlyingId.contains(":")) underlyingId.split(":").head
      else underlyingId
    }

    underlyingPrecisions.get(uid) match {
      case Some(fp) => fp
      case _ => DB.getUnderlyingDefaultPrecision(uid)
    }
  }

  def getUnderlyingRoundType(underlyingId:String):String = underlyingFixingPage.get(underlyingId) match {
    case Some(fp) => fp.roundType
    case _ => "rounded"
  }

  def getUnderlyingFixing(ul: String):UnderlyingFixingInfo = {
    if (Currencies.isForex(ul)) {
      val ccy1 = ul.take(3)
      val ccy2 = ul.takeRight(3)

      (underlyingFixingPage.get(ul), underlyingFixingPage.get(ccy2 + ccy1)) match {
        case (Some(v), _) if !v.isEmpty => UnderlyingFixingInfo(Set(v), (vs) => vs.get(v))

        case (None, Some(v)) if !v.isEmpty => UnderlyingFixingInfo(Set(v), (vs) => (vs.get(v).collect{case v => 1.0 / v}))

        case (p, _) =>
          println(s"p ${p}")

          (
            underlyingFixingPage.find{case (k, v) => k.contains(ccy1)}, 
            underlyingFixingPage.find{case (k, v) => k.contains(ccy2)},
            p
          ) match {

            case (Some((fxul1, p1)), Some((fxul2, p2)), _) if fxul1.replace(ccy1, "") == fxul2.replace(ccy2, "") && !p1.isEmpty && !p2.isEmpty =>
              UnderlyingFixingInfo(Set(p1, p2), ulFixings => (ulFixings.get(p1), ulFixings.get(p2)) match {
                case (Some(fx1), Some(fx2)) =>
                  val v1 = if (fxul1.take(3) == ccy1) fx1 else 1.0 / fx1
                  val v2 = if (fxul2.take(3) == ccy2) fx2 else 1.0 / fx2
                  Some(v1 / v2)
                case _ => None
              })
            
            case (_, _, Some(v)) => UnderlyingFixingInfo(Set(v), (vs) => vs.get(v))

            case _ => UnderlyingFixingInfo(Set.empty, (vs) => None)
          }
      }

    } else {
      underlyingFixingPage.get(ul) match {
        case Some(v) => UnderlyingFixingInfo(Set(v), (vs) => vs.get(v))
        case _ => UnderlyingFixingInfo(Set.empty, (vs) => None)
      }
    }
  }

  def computeStrike(ul:String, f:String):Option[BigDecimal]= {
    updateCompute(f).flatMap{case v => v.getDecimal(ul)(this)}
  }

  def computeStrikes(underlyingFormulas:Map[String, String]):UnderlyingFixing = {
    UnderlyingFixing(underlyingFormulas.map{case (ul, v) => (ul, computeStrike(ul, v))})
  }


}
  
object FixingInformation {
  
  def empty(
    currencyId:String,
    paymentCurrencyId:String
  ) = FixingInformation(currencyId, paymentCurrencyId, None, None, None, List.empty, DbCalendar(Set.empty))
  
}


case class UnderlyingFixingInfo(
  fixingPages:Set[FixingPage],
  priceCalculator:Map[FixingPage, Double] => Option[Double]
 ) {

  def getPrice(underlyingPrices:Map[FixingPage, Double]):Option[Double] = {
    if (fixingPages.forall(s => underlyingPrices.get(s).collect { case v => !v.isNaN }.getOrElse(false))) {
      priceCalculator(underlyingPrices)
    }
    else {
      None
    }
  }

  def getPriceFromFixings(pagePrices:Map[String, Double], isInitialFixing:Boolean):Option[Double] = {

    val underlyingPrices:Map[FixingPage, Double] =
      fixingPages.map(fixingPage => {fixingPage.getPrice(pagePrices, isInitialFixing) match {
        case Some(v) => Some((fixingPage, v))
        case _ => None
      }}).flatMap{case s => s}.toMap

    getPrice(underlyingPrices)
  }

}

case class FixingPage(
  underlying:String,
  page:Option[String],
  bidOffer:Option[String],
  time:Option[String],
  country:Option[String],
  priceType:String,
  initialPriceType:String,
  precision:Int,
  roundType:String,
  fallbackToSystem:Boolean
) {

  def isEmpty:Boolean = !page.isDefined

  val pageFull:List[String] = (page, Set(time, country).exists(_.isDefined), bidOffer) match {
    case (Some(p), true, Some("mid")) =>
      List(
        underlying + ":" + p + ":mid:" + List(time, country).flatMap(s => s).mkString(":"),
        underlying + ":" + p + ":" + List(time, country).flatMap(s => s).mkString(":")
      )
    case (Some(p), true, _) =>
      List(underlying + ":" + p + ":" + List(bidOffer, time, country).flatMap(s => s).mkString(":"))
    case _ => List.empty
  }

  val pageWithBidoffer:List[String] = (page, bidOffer) match {
    case (Some(p), Some(bo)) => List(underlying + ":" + p + ":" + bo)
    case _ => List.empty
  }

  val pageOnly:List[String] = page match {
    case Some(p) => List(underlying + ":" + p)
    case _ => List.empty
  }

  val timeOnly:List[String] = (time, country) match {
    case (Some(t), Some(c)) => List(underlying + "::" + t + ":" + c)
    case _ => List.empty
  }

  def isCloseFixing = (priceType == "close")

  def pageWithPriceType(isInitialFixing:Boolean):Option[String] = {
    val refPriceType = {
      if (isInitialFixing) initialPriceType
      else priceType
    }

    if (refPriceType == "close") None
    else Some(underlying + ":" + refPriceType.toUpperCase)
  }

//  val pageList:List[String] = List(pageFull, pageWithBidoffer, pageOnly).flatMap(s => s)

  val basePageList:List[String] = {
    if (fallbackToSystem) List(pageFull, pageWithBidoffer, pageOnly, timeOnly).flatten
    else List(pageFull, pageWithBidoffer, pageOnly).flatten
  }

  def pageList(isInitialFixing:Boolean):List[String] = pageWithPriceType(isInitialFixing) match {
    case Some(p) => p :: basePageList
    case None => basePageList
  }

  val allPageSet:Set[String] = (pageList(true) ++ pageList(false)).toSet

  override def toString = pageList(false).mkString(", ")

  def getPrice(pagePrices:Map[String, Double], isInitialFixing:Boolean):Option[Double] = {
    pageList(isInitialFixing).map(pageName => pagePrices.get(pageName)).flatMap(s => s).headOption
  }

  //  val pageList:List[String] = if (defaultPage == fallback) List(defaultPage) else List(defaultPage, fallback)

}
