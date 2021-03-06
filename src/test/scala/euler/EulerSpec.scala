package euler

import euler.Factoring._
import euler.Primes._
import euler.Sequences._
import org.scalatest.{Matchers, WordSpec}

class EulerSpec extends WordSpec with Matchers with ProblemFixture {
  "Problem one" should {
    "solve the example" in {
      multiples(10, Seq(3, 5)) shouldEqual 23
    }
    "solve the problem" in {
      multiples(1000, Seq(3, 5)) shouldEqual 233168
    }
  }
  "Problem two" should {
    "solve the problem" in {
      fib().filter(_ % 2 == 0).takeWhile(_ < 4000000).sum shouldEqual 4613732
    }
  }
  "Problem three" should {
    "solve the example" in {
      factor(13195) shouldEqual Seq(5, 7, 13, 29)
    }
    "solve the problem" in {
      factor(600851475143L).max shouldEqual 6857
    }
  }
  "Problem four" should {
    "identify palindromes" in {
      isPalindrome(9009) shouldBe true
      isPalindrome(333) shouldBe true
      isPalindrome(3434) shouldBe false
    }
    "solve the example" in {
      palindromeProduct(10, 99) shouldBe 9009
    }
    "solve the problem" in {
      palindromeProduct(100, 999) shouldBe 906609
    }
  }
  "Problem five" should {
    "solve the example" in {
      smallestMultiple(10) shouldEqual 2520
    }
    "solve the problem" in {
      smallestMultiple(20) shouldEqual 232792560
    }
  }
  "Problem six" should {
    "solve the example" in {
      sumSquareDiff(10) shouldEqual 2640
    }
    "solve the problem" in {
      sumSquareDiff(100) shouldEqual 25164150
    }
  }
  "Problem seven" should {
    "solve the example" in {
      nthPrime(6) shouldEqual 13
    }
    "solve the problem" in {
      nthPrime(10001) shouldEqual 104743
    }
  }
  "Problem eight" should {
    "not allow bad input" in {
      a [java.lang.IllegalArgumentException] should be thrownBy digitsMaxProduct("123", 5)
    }
    "solve the example" in {
      digitsMaxProduct(problem8, 4) shouldEqual 5832L
    }
    "solve the problem" in {
      digitsMaxProduct(problem8, 13) shouldEqual 23514624000L
    }
  }
  "Problem nine" should {
    "solve the problem" in {
      val triple = pythagoreanTriplets(999).find(_.sum == 1000)
      triple.get.product shouldEqual 31875000
    }
  }
}
