package euler

import euler.Primes._
import org.scalatest.{Matchers, WordSpec}

class PrimeSpec extends WordSpec with Matchers with PrimeFixture {
  "Primes" should {
    "identify primes" in {
      for (n <- 1 to oeisPrimes.last) {
        isPrime(n) shouldEqual oeisPrimes.contains(n)
      }
    }
    "generate the primes" in {
      primes.take(oeisPrimes.length) shouldEqual oeisPrimes
    }
  }
}
