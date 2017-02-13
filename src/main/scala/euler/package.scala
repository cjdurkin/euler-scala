import java.lang.Math.sqrt

import scala.math._

package object euler {

  def isPalindrome(a: Int): Boolean = a.toString.reverse == a.toString

  def palindromeProduct(lower: Int, upper: Int): Int = {
    val loop = for {
      a <- lower to upper
      b <- lower to upper
      c = a * b
      if isPalindrome(c)
    } yield c
    loop.max
  }

  def sumSquareDiff(stop: Long, start: Long = 1): Long = {
    val sumSquares = (start to stop).map(pow(_, 2).toLong).sum
    val squareSum = pow((start to stop).sum, 2).toLong
    squareSum - sumSquares
  }

  def digitsMaxProduct(largeNumber: String, digits: Int): Long = {
    require(largeNumber.length >= digits, "not enough digits")
    def dmp(n: String, d: Int): Stream[Long] = {
      if (n.length < d) {
        Stream.empty[Long]
      } else {
        val headProduct = n.take(d).map(_.toString.toLong).product
        headProduct #:: dmp(n.substring(1), d)
      }
    }
    dmp(largeNumber, digits).max
  }

  case class Triple(a: Int, b: Int, c: Int) extends Iterable[Int] {
    override def iterator: Iterator[Int] = Seq(a, b, c).iterator
  }

  def pythagoreanTriplets(max: Int): Seq[Triple] = {
    for {
      a <- 1 to max
      b <- 1 to max
      c = sqrt(a * a + b * b)
      if c.isWhole
    } yield Triple(a, b, c.toInt)
  }
}
