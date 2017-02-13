package euler

import euler.Factoring._
import org.scalatest.{Matchers, WordSpec}

class FactoringSpec extends WordSpec with Matchers {
  "Factoring" should {
    "factor numbers" in {
      factor(24) shouldEqual Seq(2, 2, 2, 3)
      factor(17) shouldEqual Seq(17)
    }
    "find multiples" in {
      multiples(1000, Seq(3, 5)) shouldEqual 233168
      multiplesBrute(1000, Seq(3, 5)) shouldEqual 233168
    }
    "check divisors" in {
      hasAllDivisors(10, Seq(1,2,5)) shouldBe true
      hasAllDivisors(10, Seq(2,3,5)) shouldBe false
    }
  }
}
