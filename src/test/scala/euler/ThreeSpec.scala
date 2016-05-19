package euler

import org.scalatest.{Matchers, WordSpec}


class ThreeSpec extends WordSpec with Matchers {
  "Problem three solution" should {
    "identify primes" in {
      isPrime(2) shouldBe true
      isPrime(24) shouldBe false
      isPrime(31) shouldBe true
      isPrime(33) shouldBe false
    }
    "factor primes" in {
      factor(24).toSeq shouldEqual Seq(2,2,2,3)
      factor(17).toSeq shouldEqual Seq(17)
    }
    "do example" in {
      factor(13195).toSeq shouldEqual Seq(5,7,13,29)
    }
    "solve problem" in {
      factor(600851475143L).max shouldEqual 6857
    }
  }
}
