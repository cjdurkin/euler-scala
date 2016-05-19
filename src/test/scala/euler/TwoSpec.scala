package euler

import org.scalatest.{Matchers, WordSpec}


class TwoSpec extends WordSpec with Matchers {
  "Problem two solution" should {
    "correctly solve the problem" in {
      fib().filter(_ % 2 == 0).takeWhile(_ < 4000000).sum shouldEqual 4613732
    }
  }
}
