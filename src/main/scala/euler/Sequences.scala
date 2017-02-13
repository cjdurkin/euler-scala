package euler

object Sequences {
  def countUp(start: Int, step: Int): Stream[Int] = start #:: countUp(start + step, step)

  def fib(a: Long = 1, b: Long = 1): Stream[Long] = a #:: fib(b, a + b)
}
