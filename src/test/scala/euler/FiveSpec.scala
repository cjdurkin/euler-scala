package euler

import org.scalatest.{Matchers, WordSpec}


class FiveSpec extends WordSpec with Matchers{
  "Problem five solution" should {
    "solve the example" in {
      Stream.from(1).map(_ * 10).find(hasAllDivisors(_, 1 to 10)) shouldEqual Some(2520)
    }
    "solve the problem" in {
      Stream.from(1).map(_ * 20).find(hasAllDivisors(_, 1 to 20)) shouldEqual Some(232792560)
    }
  }
}
