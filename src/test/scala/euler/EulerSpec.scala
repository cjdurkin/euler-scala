package euler

import org.scalatest.{Matchers, WordSpec}


class EulerSpec extends WordSpec with Matchers {
  "Problem one solution" should {
    "correctly solve the example" in {
      multiples(10, Seq(3, 5)).sum shouldEqual 23
    }
    "correctly solve the problem" in {
      multiples(1000, Seq(3, 5)).sum shouldEqual 233168
    }
  }
  "Problem two solution" should {
    "correctly solve the problem" in {
      fib().filter(_ % 2 == 0).takeWhile(_ < 4000000).sum shouldEqual 4613732
    }
  }
  "Problem three solution" should {
    "identify primes" in {
      isPrime(2) shouldBe true
      isPrime(24) shouldBe false
      isPrime(31) shouldBe true
      isPrime(33) shouldBe false
    }
    "factor primes" in {
      factor(24).toSeq shouldEqual Seq(2, 2, 2, 3)
      factor(17).toSeq shouldEqual Seq(17)
    }
    "do example" in {
      factor(13195).toSeq shouldEqual Seq(5, 7, 13, 29)
    }
    "solve problem" in {
      factor(600851475143L).max shouldEqual 6857
    }
  }
  "Problem four solution" should {
    "identify palindromes" in {
      isPalindrome(9009) shouldBe true
      isPalindrome(333) shouldBe true
      isPalindrome(3434) shouldBe false
    }
    "solve the example" in {
      palinProduct(10, 99) shouldBe 9009
    }
    "solve the problem" in {
      palinProduct(100, 999) shouldBe 906609
    }
  }
  "Problem five solution" should {
    "solve the example" in {
      Stream.from(1).map(_ * 10).find(hasAllDivisors(_, 1 to 10)) shouldEqual Some(2520)
    }
    "solve the problem" in {
      Stream.from(1).map(_ * 20).find(hasAllDivisors(_, 1 to 20)) shouldEqual Some(232792560)
    }
  }
  "Problem six solution" should {
    "solve the example" in {
      sumSquareDiff(10) shouldEqual 2640
    }
    "solve the problem" in {
      sumSquareDiff(100) shouldEqual 25164150
    }
  }
}
