import Math.sqrt

package object euler {
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

  def hasAllDivisors(a: Int, divisors: Seq[Int]): Boolean = !divisors.exists(a % _ != 0)
}
