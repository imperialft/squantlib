import squantlib.math.FormulaInterpreter

val f = "USDJPY / USDEUR"
val formula = new FormulaInterpreter(f)
val fixings = Map("USDJPY" -> 10.0, "EURUSD" -> 10.0, "CMT10" -> 0.05)


val bond = DB.getBonds(List("JGBR-0023")).head
val interp = new squantlib.math.FormulaInterpreter(bond.coupon)
