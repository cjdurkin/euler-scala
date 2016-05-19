package euler

import org.scalatest.{Matchers, WordSpec}


class OneSpec extends WordSpec with Matchers {
  "Problem one solution" should {
    "correctly solve the example" in {
      One.multiples(10, Seq(3,5)).sum shouldEqual 23
    }
    "correctly solve the problem" in {
      One.multiples(1000, Seq(3,5)).sum shouldEqual 233168
    }
  }
}
