package euler

import org.scalatest.{Matchers, WordSpec}
import euler.Four.palinProduct

class FourSpec extends WordSpec with Matchers {
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
}
