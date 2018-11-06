package net.squantlib.schedule.payoff

import java.util.{Map => JavaMap}

import net.squantlib.schedule.CalculationPeriod
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.{Date, FixingInformation}


class PriceResult {

  protected var price: Option[Double] = None
  protected var assetId: Option[String] = None
  protected var assetPrice: Option[Double] = None

  def setPrice(p:Double):Unit = {
    if (!p.isNaN && !p.isInfinity) price = Some(p)
  }

  def getPrice:Option[Double] = price

  def setAssetInfo(aid:String, p:Double):Unit = {
    if (!p.isNaN && !p.isInfinity) {
      assetId = Some(aid)
      assetPrice = Some(p)
    }
  }

  def deleteAssetInfo:Unit = {
    assetId = None
    assetPrice = None
  }

  def getAssetId:Option[String] = assetId

  def getAssetPrice:Option[Double] = assetPrice

  override def toString = Map("price" -> price, "assetId" -> assetId, "assetPrice" -> assetPrice).map{case (k, v) => s"${k} : ${v.getOrElse("None").toString}"}.mkString(", ")

}


