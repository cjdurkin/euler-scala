import java.lang.Math.sqrt

import scala.math._

package object euler {
  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  def lcm(a: Long, b: Long): Long = a * b / gcd(a, b)

  def isMult(num: Int, divisors: Seq[Int]): Boolean = divisors.exists(num % _ == 0)

  def multiples(limit: Int, numbers: Seq[Int]): Int = (1 until limit).filter(isMult(_, numbers)).sum

  def multiples2(limit: Int, numbers: Seq[Int]): Int = {
    val streams = for (n <- numbers) yield n.until(limit, n)
    streams.foldLeft(Stream.empty[Int])(_ ++ _).distinct.sum
  }

  def fib(a: Long = 1, b: Long = 1): Stream[Long] = a #:: fib(b, a + b)

  def factor(a: Long): Stream[Long] = {
    firstFactor(a) match {
      case Some(f) => f #:: factor(a / f)
      case None => a #:: Stream.empty
    }
  }

  def firstFactor(a: Long): Option[Long] = (2L to sqrt(a).toLong).find(a % _ == 0)

  def isPrime(a: Long): Boolean = !(2L to sqrt(a).toLong).exists(a % _ == 0)

  def isPalindrome(a: Int): Boolean = a.toString.reverse == a.toString

  def palinProduct(lower: Int, upper: Int): Int = {
    val loop = for {a <- lower to upper
                    b <- lower to upper
                    c = a * b
                    if isPalindrome(c)
    } yield c
    loop.max
  }

  def hasAllDivisors(a: Int, divisors: Seq[Int]): Boolean = !divisors.exists(a % _ != 0)

  def smallestMultiple(a: Long): Long = (1L to a).reduce(lcm(_, _))

  def sumSquareDiff(stop: Long, start: Long = 1): Long = {
    val sumSquares = (start to stop).map(pow(_, 2).toLong).sum
    val squareSum = pow((start to stop).sum, 2).toLong
    squareSum - sumSquares
  }
}
