package squantlib.database.schemadefinitions

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity
import squantlib.setting.initializer.Calendars
import org.jquantlib.time.Calendar
import squantlib.util.JsonUtils._
import scala.collection.JavaConversions._

class Bond(@Column("ID")					var id: String,
              @Column("REF_NUMBER")			var ref_number: Int,
              @Column("FILING")				var filing: Date,
              @Column("ISSUEDATE")			var issuedate: Date,
              @Column("MATURITY")			var maturity: Date,
              @Column("NOMINAL")			var nominal: Option[Double],
              @Column("DENOMINATION")		var denomination: Option[Double],
              @Column("UNDERLYING")			var underlying: String,
              @Column("UNDERLYINGINFO")		var underlyinginfo: String,
              @Column("COUPON")				var coupon: String,
              @Column("COUPON_FREQ")		var coupon_freq: Option[Int],
              @Column("DAYCOUNT")			var daycount: String,
              @Column("DAYCOUNT_ADJ")		var daycount_adj: String,
              @Column("PAYMENT_ADJ")		var payment_adj: String,
              @Column("CALENDAR")			var calendar_str: String,
              @Column("INARREARS")			var inarrears: Option[Int],
              @Column("CPNNOTICE")			var cpnnotice: Option[Int],
              @Column("ISSUEPRICE")			var issueprice: Option[Double],
              @Column("REDEMPRICE")			var redemprice: String,
              @Column("REDEMNOTICE")		var redemnotice: Option[Int],
              @Column("CALL")				var call: String,
              @Column("TYPE")				var bondtype: String,
              @Column("INITIALFX")			var initialfx: Double,
              @Column("FIXINGDATE")			var fixingdate: Option[Date],
              @Column("FIXINGS")			var fixings: String,
              @Column("ISIN")				var isin: String,
              @Column("TICKER")				var ticker: String,
              @Column("LEADMANAGER")		var leadmanager: String,
              @Column("BONDNAME")			var bondname: String,
              @Column("SHORTNAME")			var shortname: String,
              @Column("EDINETNAME")			var edinetname: String,
              @Column("DESCRIPTION_JPN")	var description_jpn: String,
              @Column("DESCRIPTION_ENG")	var description_eng: String,
              @Column("CurrencyID")			var currencyid: String,
              @Column("ProductID")			var productid: String,
              @Column("IssuerID")			var issuerid: String,
              @Column("RISKTAGS") 			var risktags: String,
              @Column("SETTINGS") 			var settings: String,
              @Column("PRICETAG") 			var pricetag: Option[Int],
              @Column("Created")			var created: Option[Date],
              @Column("LastModified")		var lastmodified : Option[Date]
              ) extends KeyedEntity[String] {

  
  def calendar:Calendar = {
    val cdrlist:Set[String] = calendar_str.split(",").map(_.trim).toSet
    Calendars(cdrlist).getOrElse(Calendars(currencyid).get)
  }
  
  private def getFieldMap: Map[String, Any] = {
    val fieldsAsPairs = for (field <- this.getClass.getDeclaredFields) yield {
      field.setAccessible(true)
	  (field.getName, field.get(this))
	  }
	  Map(fieldsAsPairs :_*)
	}
  
  private def emptyToNull(s:Any) = s match {
    case x:String => if (x != null && x.trim.isEmpty) null else s
    case x => x
  }
  
  private def objCompare(a:Any, b:Any) = emptyToNull(a) == emptyToNull(b)
  
  private def compareMap(m1:Map[String, Any], m2:Map[String, Any]):Boolean = m1.forall{
    case ("lastmodified", _) | ("created", _) | ("initialfx", _) | ("fixings", _)=> true
    case (k, _) if k.head == '_' => true
    case (k, v) => m2.get(k) match {
      case Some(vv) => objCompare(v, vv)
      case None => false
      }
    }
  
  def isSameContent(b:Bond):Boolean = {
    compareMap(getFieldMap, b.getFieldMap)
  }
  
  def couponList:List[String] = coupon.parseJsonStringList.map(_.orNull)
  def underlyingList:List[String] = underlying.parseJsonStringList.map(_.orNull)
  
  def bermudanList:List[Boolean] = call.parseJsonStringList.map(_.orNull == "berm")
  def triggerList:List[List[String]] = call.jsonArray.map(_.parseStringList.map(_.orNull))
  
  def fixingList:Map[String, Double] = fixings.parseJsonDoubleFields
  def descriptionjpnList:Map[String, String] = description_jpn.parseJsonStringFields
  def descriptionengList:Map[String, String] = description_eng.parseJsonStringFields
  
  def this() = this(
		id = null,
		ref_number = 0,
		filing = new Date,
		issuedate = new Date,
		maturity = new Date,
		nominal = Some(-9999),
		denomination = Some(-999.0),
		underlying = null,
		underlyinginfo = null,
		coupon = null,
		coupon_freq = Some(9999),
		daycount = null,
		daycount_adj = null,
		calendar_str = null, 
		payment_adj = null,
		inarrears = Some(0),
		cpnnotice = Some(0),
		issueprice = Some(-999.0),
		redemprice = null,
		redemnotice = Some(0),
		call = null,
		bondtype = null,
		initialfx = 0.0,
		fixingdate = None,
		fixings = null,
		isin = null,
		ticker = null,
		leadmanager = null,
		bondname = null,
		shortname = null,
		edinetname = null,
		description_jpn = null,
		description_eng = null, 
		currencyid = null,
		productid = null,
		issuerid = null,
		risktags = null,
		settings = null,
		pricetag = Some(-9999),
		created = None,
		lastmodified  = None)
 
  override def toString():String = format("%-5s %-15s %-25s %-10s %-15s %-15s", id, issuedate.toString, maturity.toString, coupon, initialfx.toString, created.toString)
  
}