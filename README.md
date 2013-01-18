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
* Computing forward prices
* Price exotic FX options using BS and Montecarlo
* Computing historical volatilities and correlations.
* Pricing FX linked bonds (dual currency, callable dual currency, prdc)

###Bond
* Building flexible cashflows. (fix, binary, linear sums, put, cap, floor, etc)
* Pricing bonds with specific pricing mechanisms.
* Risk analysis - delta (rate & fx), vega (rate & fx), yield, durations, convexity, etc..

###To Do (maybe)
* Equity and index linked products
* Support for bonds with published prices
* More sophisticated montecarlo model (smile, multi-factor)
* Proper Bermudan swaptions

## What is this for?
* Main pricing backbone behind PFOL.IO project (http://pfol.io)
* Free for personal use - any interest?
 
## Contact
Masakatsu Wakayu (masakatsu.wakayu@imperial-ft.com)
Imperial Finance & Technology (http://www.imperial-ft.com)
