package squantlib.payoff

import squantlib.payoff._

object PayoffExamples {
  
	val fixed = Payoff("0.5%")
	
	val fixed_json = Payoff("""
	    {"type" : "fixed",
	    "payoff" : "0.5%",
	    "description" : "0.5% fixed coupon"
	    }""")
	    
	
	val linear1d = Payoff("12.5% * usdjpy / 80.2 - 10% > 0.5% < 3%")
	
	val linear1d_json = Payoff("""
	    {"type" : "linear1d", 
	    "variable" : "usdjpy", 
	    "payoff" : 
			{"min" : "0.5%", 
			"max" : "3.00%", 
			"mult" : 0.00155860349127182, 
			"add" : "-10%", 
			"description" : "PRDC: 12.5% * FX/80.2 - 10%"}
	    }""")
	
	    
	val leps1d = Payoff("leps -12.5% * usdjpy / 80.2 - 10% > 68 < 80, 200% * usdjpy / 80.2 - 100% > 75 < 95")
	
	val leps1d_json = Payoff("""
	    {"type" : "leps1d", 
	    "variable" : "usdjpy", 
	    "description" : "Double PRDC formula", 
	    "payoff" : [
			{"minrange" : 60, "maxrange" : 80, "mult" : -0.00155860349127182, "add" : "-10%"}, 
			{"minrange" : 75, "maxrange" : 95, "mult" : 0.0249376558603491, "add" : "-100%"}]
	    }""")

	    
	 val fixseries = Payoffs("0.005; 0.6%; 0.7%; 0.008")
	 
	 val fixseries_json = Payoffs("""
	     {"type" : "fixedseries", 
	     "variable" : "usdjpy", 
	     "payoff" : ["0.5%", 0.006, "0.7%"]}
	     """)
	 
	 val linear1dseries = Payoffs("12.5% * usdjpy/80.2 - 10% > 0.5% < 3%; 20% * usdjpy/80.2 - 10% > 0% <10%")
	 
	 val linear1dseries_json = Payoffs("""
	     {"type" : "linear1dseries", 
	     "variable" : "usdjpy", 
	     "payoff" : [
			 {"min" : "0.5%", 
			 "max" : "3.00%", 
			 "mult" : 0.00155860349127182, 
			 "add" : "-10%", 
			 "description" : "PRDC: 12.5% * FX/FX0 - 10%"}, 
			 {"min" : "0.5%", 
		     "max" : "10.00%", 
		     "mult" : 0.00249376558603491, 
		     "add" : "-10%", 
		     "description" : "PRDC: 20% * FX/FX0 - 10%"}
			 ]}""")
			 
	  val leps1dseries = Payoffs("leps -12.5% * usdjpy/80.2 - 10% > 68 < 80, 200% * usdjpy / 80.2 - 100% > 75 < 95;  leps 12.5% * usdjpy / 80.2 - 10% > 68 < 80, -200% * usdjpy / 80.2 - 100% > 75 < 95")

	  val leps1dseries_json = Payoffs("""
	      {"type" : "leps1dseries", 
	      "variable" : "usdjpy", 
	      "legs" : [
			  {"description":"Dual PRDC -12.5%-10% & 200% - 100%", 
			  "payoff":[
			  	{"minrange" : 68, "maxrange" : 80, "mult" : -0.00155860349127182, "add" : "-10%"}, 
			  	{"minrange" : 75, "maxrange" : 95, "mult" : 0.0249376558603491, "add" : "-100%"}]}, 
			  {"description":"Dual PRDC 12.5%-10% & -200% - 100%", 
			  "payoff":[
			  	{"minrange" : 68, "maxrange" : 80, "mult" : 0.00155860349127182, "add" : "-10%"}, 
			  	{"minrange" : 75, "maxrange" : 95, "mult" : -0.0249376558603491, "add" : "-100%"}]}]}
	      """)

	  val fixedleg = Payoff("0.5%")
	  val linear1dleg = Payoff("12.5% * usdjpy / 80.2 - 10% > 0.5% < 3%")
      val combileg = Payoffs(List(fixedleg, linear1dleg))

}


