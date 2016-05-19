package euler


class One {

}

object One {
  def ismult(num: Int, divisors: Seq[Int]): Boolean = {
    for (c <- divisors) {
      if (num % c == 0) {
        return true
      }
    }
    false
  }

  def multiples(limit: Int, numbers: Seq[Int]) = {
    for (c <- 1 until limit if ismult(c, numbers)) yield {
      c
    }
  }
}
