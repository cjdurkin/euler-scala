package euler

import java.lang.Math._

object Primes {
  /**
    * Brute force test for primality.
    *
    * @param a
    * @return true if number is prime
    */
  def isPrime(a: Long): Boolean = if (a < 2) false else !(2L to sqrt(a).toLong).exists(a % _ == 0)

  /**
    * Stream of integers including the primes less than ten, then integers ending with 1, 3, 7, or 9 (omitting
    * multiples of 2, 5, and 10, which cannot be prime).
    *
    * @return Stream of numbers which includes all primes but also some composite numbers
    */
  def candidatePrimes: Stream[Int] = {
    def cPrime(n: Int): Stream[Int] = n #:: (n + 2) #:: (n + 6) #:: (n + 8) #:: cPrime(n + 10)
    2 #:: 3 #:: 5 #:: 7 #:: cPrime(11)
  }

  /**
    * @return Stream of prime numbers
    */
  def primes: Stream[Int] = candidatePrimes.filter(isPrime(_))

  /**
    * @param n
    * @return the Nth prime number
    */
  def nthPrime(n: Int): Int = {
    primes.drop(n - 1).head
  }
}