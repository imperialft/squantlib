package squantlib.montecarlo.payoff

import squantlib.setting.initializer.Currencies
import scala.collection.mutable.{Map => mutableMap}

/**
 * Interprets general formula as the sum of weighted variables + constant, with cap and floor.
 * Any symbols other than +, -, *, /, > and < are considered as an independent variable.
 * Brackets are not supported.
 */
case class GeneralPayoff(val formula:String) extends Payoff {
  
	val (parsedFormula:Map[Set[String], Double], 
	     floor:Option[Double], 
	     cap:Option[Double]) = GeneralPayoff.split(formula)
	
	val variables:Set[String] = parsedFormula.keySet.flatten
	 
	def leverage(index:String):Double = leverage(Set(index))
	def leverage(index:Set[String]):Double = parsedFormula.getOrElse(index, 0.0)
	
	def constant:Double = parsedFormula.getOrElse(Set.empty, 0.0)
	
	override def price(fixings:List[Map[String, Double]]) = fixings.map(price)
	
	override def price(fixings:List[Double]) (implicit d:DummyImplicit) = 
	  if (variables.size == 1) fixings.map(f => price(Map(variables.head -> f)))
	  else List.empty
	
	override def price(fixings:Map[String, Double]):Option[Double] = {
	  if (!variables.forall(x => fixings.contains(x))) return None
	  
	  var rate = parsedFormula.map{
      	case (vs, c) if (vs.isEmpty) => 1.0
      	case (vs, c) => vs.toList.map(x => fixings(x)).product * c
	  }.sum
    
      if (floor.isDefined) rate = rate.max(floor.get)
      if (cap.isDefined) rate = rate.min(cap.get)
	
      Some(rate)
	}
	
	override def price = List.empty
	
	override def toString:String = parsedFormula.map{
	  case (vs, c) if (vs.isEmpty) => "Const"
	  case (vs, c) => vs.reduce((x, y) => x + ", " + y) + ", " + c
	}.reduce((x, y) => x + "\n" + y)
	
	override def legs = 1

}

object GeneralPayoff {
  
  /**
   * Parse a linear formula string into sum of named variables
   * returns tuple containing
   * 1) Cashflow as map(variable names -> leverage)
   * 2) Floor (ie. minimum amount)
   * 3) Cap (ie. maximum amount)
   */
    def split(cashflow:String):(Map[Set[String], Double], Option[Double], Option[Double]) = {
      if (cashflow.isEmpty) return (Map.empty, None, None)
      
      var coeff:mutableMap[Set[String], Double] = mutableMap.empty
      var cap:Option[Double] = None
      var floor:Option[Double] = None
      
      var rateformula = (if(cashflow.trim.head == '-') "0" else "") + replacestr(cashflow, 
          Map(" " -> "", "%" -> "", "\r" -> "", "\n" -> "", "-" -> "+-1*", ">" -> "+>", "<" -> "+<"))

      rateformula.split('+').filter(!_.isEmpty).foreach{ sumcomponent => 
        sumcomponent match {
          case s if (s.head == '<') => {
	        cap = parseDouble(s.tail).collect {
	            case v => cap match {
	              case None => v/100.0
	              case Some(c) => c.min(v/100.0)
	      }}}
          
          case s if (s.head == '>') => {
            floor = parseDouble(s.tail).collect {
              case v => floor match {
	          	case None => v/100.0
	          	case Some(f) => f.max(v/100.0)
	      }}}
          
          case s => {
            val splitmult = s.replace("/", "*/").split('*').filter(!_.isEmpty)
		    var c = 1.00
		    var varnames = Set.empty[String]
		    
		    splitmult.foreach{ productcomponent => 
		      productcomponent match {
		        case t if (t.head == '/') => {
		          val valuepart = t.tail
		          parseDouble(valuepart) match {
		            case Some(v) => { c /= v }
		            case None => {
		              if (isFX(valuepart)) varnames += flipFX(valuepart) 
		              else varnames += t
		        }}}
		        
		        case t => {
		          parseDouble(t) match {
		            case Some(v) => c = c * v
		            case None => varnames += t
		        }}}
		      }
		        
		      if (varnames.isEmpty) c /= 100
		      if (coeff.contains(varnames)) coeff(varnames) += c
		      else coeff.update(varnames, c)
	        }
	     }}

      (Map(coeff.toSeq:_*), floor, cap)
    }
    
  /**
   * Parse string into double. Returns Null if string is not a number.
   */
	def parseDouble(s: String):Option[Double] = try { Some(s.toDouble) } catch { case _ => None }
	
	private def isFX(s:String):Boolean = 
	  (s != null && s.length == 6 && Currencies.contains(s.substring(0, 3)) && Currencies.contains(s.substring(3, 6)))
	  
	private def flipFX(s:String):String = s.substring(3, 6) + s.substring(0, 3)

	private def replacestr(s:String, replacef:Map[String, String]):String = {
	  var resultstring = s
	  replacef.keySet.foreach{r => {
	    resultstring = resultstring.replace(r, replacef(r))
	  }}
	  resultstring
	}
}

