/**
 * Generates some sequence of random numbers.
 *
 */

import java.io.PrintWriter
import squantlib.math.random._

val file = new PrintWriter("random.txt")
file.println("Mersenne Twister (sum 12 samples minus 6)")
new MersenneTwister(20120808).take(1000000).foreach(file.println)

file.println("java.util.Random (sum 12 samples minus 6)")
new Java(20120808).take(1000000).foreach(file.println)
file.close()