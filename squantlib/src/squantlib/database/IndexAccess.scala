package squantlib.database

import squantlib.initializer.Initializer
import java.util.{Date => JavaDate}

/**
* Functions to access from database by index name
*/
object IndexTimeSeries {

	def apply(id:String):Option[Map[JavaDate, Double]] = mapper.get(id)
	def getOrElse(id:String, defaultvalue:Map[JavaDate, Double]):Map[JavaDate, Double] = apply(id).getOrElse(defaultvalue)
	def contains(s:String):Boolean = mapper.contains(s)
	def keySet:Set[String] = mapper.keySet
  
	val yearunits = Set(1, 2, 3, 4, 5, 7, 10, 15, 20, 30)
	val cmsmapper = yearunits.map(y => ("CMS" + y, DB.getTimeSeries("Swap", "JPY", y + "Y"))).toMap;
	
	val monthunits = Set(1, 2, 3, 6, 12)
	val libormapper = monthunits.map(m => ("LIB" + m + "M", DB.getTimeSeries("Cash", "JPY", m+"M"))).toMap;

	val cmtmapper = Map(("CMT10" -> DB.getTimeSeries("Fixing", "JGBY", "10Y"))).toMap;
	
	val mapper = cmsmapper ++ libormapper ++ cmtmapper
	
}


/**
* Functions to access from database by index name
*/
object IndexValue{

	def apply(id:String):Option[(JavaDate, Double)] = apply(id, new JavaDate)
	  
	def apply(id:String, vd:JavaDate):Option[(JavaDate, Double)] = {
		if (mapper.contains(id)) mapper(id)(vd)
		else None
	}
	def getOrElse(id:String, vd:JavaDate, defaultvalue:(JavaDate, Double)):(JavaDate, Double) = apply(id, vd).getOrElse(defaultvalue)
	def contains(s:String):Boolean = mapper.contains(s)
	def keySet:Set[String] = mapper.keySet
  
	val yearunits = Set(1, 2, 3, 4, 5, 7, 10, 15, 20, 30)
	val cmsmapper = yearunits.map(y => ("CMS" + y, (d:JavaDate) => DB.getLatestParam("Swap", "JPY", y + "Y", d))).toMap;
	
	val monthunits = Set(1, 2, 3, 6, 12)
	val libormapper = monthunits.map(m => ("LIB" + m + "M", (d:JavaDate) => DB.getLatestParam("Cash", "JPY", m+"M", d))).toMap;

	val cmtmapper = Map(("CMT10" -> ((d:JavaDate) => DB.getLatestParam("Fixing", "JGBY", "10Y", d)))).toMap;
	
	val mapper = cmsmapper ++ libormapper ++ cmtmapper
}
