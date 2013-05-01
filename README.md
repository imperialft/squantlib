SquantLib
=========

*SquantLib* is a financial engineering tool focused on pricing Bonds written in Scala.

**Helps, suggestions and bug-fix are very much appreciated!!**

## What can it do?
###Rates
* Building yield curves from given market quotes.
* Drawing charts of various curves.
* Computing cashflow discount curve using given credit spread, either from yield curves or FX forward curve.
* Building Swaption volatility surface
* Pricing swaptions
* Pricing bonds (vanilla, callable, step-up)

###FX
* Building volatility surfaces
* Compute forward
* Price exotic FX options using BS and Montecarlo
* Computing historical volatilities and correlations.
* Pricing FX linked bonds with trigger using Black-Scholes MC
* Pricing FX linked bonds with callability through equivalent trigger approximation 

###Bond
* Building various cashflows. (fix, binary, linear sums, put, cap, floor, etc)
* Pricing bonds with specific pricing mechanisms.
* Greeks - delta (rate & fx), vega (rate & fx), yield, durations, convexity, etc..
* Consolidate bonds with published prices

###Indices
* Pricing index linked bond with trigger

###To Do (maybe)
* Equity linked products
* More sophisticated montecarlo model (smile, multi-factor)
* Bermudan swaptions

## What is this for?
* Main pricing backbone behind PFOL.IO project (http://pfol.io)
* Free for personal use - any interest?
 
## Contact
Masakatsu Wakayu (masakatsu.wakayu@imperial-ft.com)
Imperial Finance & Technology (http://www.imperial-ft.com)
