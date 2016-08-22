package euler

/**
  * Created by cjd on 5/19/16.
  */
class Four {

}

object Four {
  def palinProduct(lower: Int, upper: Int): Int = {
    val loop = for {a <- lower to upper
         b <- lower to upper
         c = a * b
         if isPalindrome(c)
    } yield c
    loop.max
  }
}