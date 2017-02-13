package euler

import euler.Sequences._
import org.scalatest.{Matchers, WordSpec}

class SequenceSpec extends WordSpec with Matchers {
  "Sequences" should {
    "count up" in {
      countUp(1, 1).take(5) shouldEqual Seq(1, 2, 3, 4, 5)
      countUp(3, 2).take(10) shouldEqual Seq(3, 5, 7, 9, 11, 13, 15, 17, 19, 21)
    }
    "generate fibonacci numbers" in {
      fib().take(8) shouldEqual Seq(1, 1, 2, 3, 5, 8, 13, 21)
    }
  }
}
