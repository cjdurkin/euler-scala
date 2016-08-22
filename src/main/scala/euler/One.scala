package euler


class One {

}

object One {
  def isMult(num: Int, divisors: Seq[Int]): Boolean = divisors.exists(num % _ == 0)

  def multiples(limit: Int, numbers: Seq[Int]) = (1 until limit).filter(isMult(_, numbers))
}
