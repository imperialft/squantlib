package squantlib.initializer

import org.jquantlib.indexes.{JpyJGBYieldIndex, Index}
import org.jquantlib.indexes.JpyLiborSwapIsdaFixPm
import org.jquantlib.indexes.ibor.JPYLibor
import org.jquantlib.time.{Period => qlPeriod, TimeUnit}
import org.jquantlib.indexes.BondYieldIndex
import org.jquantlib.indexes.SwapIndex
import org.jquantlib.indexes.IborIndex

import TimeUnit._

object Indices extends Initializer[Index] {
	val mapper = BondYieldIndices.mapper ++ CMSIndices.mapper ++ LiborIndices.mapper

}

object BondYieldIndices extends Initializer[BondYieldIndex] {
	val mapper = Map(("CMT10" -> new JpyJGBYieldIndex(new qlPeriod(10, Years))))
}

object CMSIndices extends Initializer[SwapIndex] {
	val yearunits = Set(1, 2, 3, 4, 5, 7, 10, 15, 20, 30)
	val mapper = yearunits.map(y => ("CMS" + y, new JpyLiborSwapIsdaFixPm(new qlPeriod(y, Years)))).toMap;
} 

object LiborIndices {
	val monthunits = Set(1, 2, 3, 6, 12)
	val mapper = monthunits.map(m => ("LIB" + m + "M", new JPYLibor(new qlPeriod(m, Months)))).toMap;
}

