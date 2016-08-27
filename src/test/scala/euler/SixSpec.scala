package euler

import org.scalatest.{Matchers, WordSpec}
import euler.Six.sumSquareDiff


class SixSpec extends WordSpec with Matchers {
  "Problem six solution" should {
    "solve the example" in {
      sumSquareDiff(10) shouldEqual 2640
    }
    "solve the problem" in {
      sumSquareDiff(100) shouldEqual 25164150
    }
  }
}
