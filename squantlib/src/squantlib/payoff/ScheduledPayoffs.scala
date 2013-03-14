package squantlib.payoff

import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import scala.collection.LinearSeq
import scala.annotation.tailrec 
import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._
import scala.collection.JavaConversions._
import org.jquantlib.time.{Date => qlDate}

case class ScheduledPayoffs(payoffs:Payoffs, schedule:Schedule) {
  
  val scheduledPayoffs = (schedule zip payoffs)
  
  val eventDateLegs:List[List[qlDate]] = scheduledPayoffs.map{case (d, p) => p.eventDates(d)}.toList
  
  val eventDate:List[qlDate] = eventDateLegs.flatten.toSet.toList.sorted
  
  val dateMapper:List[List[Int]] = eventDateLegs.map(_.map(eventDate.indexOf(_)))
  
  def priceMapper[T](fixings:List[T]):List[List[T]] = dateMapper.map(_.map(fixings))
  
  def price(fixings:List[Double])(implicit d:DummyImplicit):List[Double] = {
    val f:List[List[Double]] = priceMapper(fixings)
    payoffs.price(f)
  }

  def price(fixings:List[Map[String, Double]]):List[Double] = 
    payoffs.price(priceMapper(fixings))
    
  def price(fixings:List[Map[String, Double]], trigger:List[Option[Map[String, Double]]], trigAmount:List[Double]):List[Double] = 
    payoffs.price(priceMapper(fixings), trigger, trigAmount)
    
  def price(fixings:List[Double], trigger:List[Option[Double]], trigAmount:List[Double])(implicit d:DummyImplicit):List[Double] = 
    payoffs.price(priceMapper(fixings), trigger, trigAmount)

    

}


