package euler

import java.lang.Math._

object Factoring {
  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  def lcm(a: Long, b: Long): Long = a * b / gcd(a, b)

  def isMultiple(num: Int, divisors: Seq[Int]): Boolean = divisors.exists(num % _ == 0)

  /**
    * Find all numbers below a limit that are a multiples of any of the given numbers, and sum them.
    */
  def multiplesBrute(limit: Int, numbers: Seq[Int]): Int = (1 until limit).filter(isMultiple(_, numbers)).sum

  /**
    * Alternative way to calculate multiples. Construct a list of multiples below the limit for each given number,
    * combine, remove duplicates, then sum.
    */
  def multiples(limit: Int, numbers: Seq[Int]): Int = {
    val streams = for (n <- numbers) yield n.until(limit, n)
    streams.foldLeft(Stream.empty[Int])(_ ++ _).distinct.sum
  }

  def firstFactor(a: Long): Option[Long] = (2L to sqrt(a).toLong).find(a % _ == 0)

  def factor(a: Long): Stream[Long] = {
    firstFactor(a) match {
      case Some(f) => f #:: factor(a / f)
      case None => a #:: Stream.empty
    }
  }

  def hasAllDivisors(a: Int, divisors: Seq[Int]): Boolean = !divisors.exists(a % _ != 0)

  /**
    * Find the smallest number that is a multiple of all integers up the given number.
    */
  def smallestMultiple(a: Long): Long = (1L to a) reduce lcm
}
