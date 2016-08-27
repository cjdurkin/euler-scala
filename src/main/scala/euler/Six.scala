package euler
import math.pow

class Six {

}

object Six {

  def sumSquareDiff(stop: Long, start: Long = 1): Long = {
    val sumSquares = (start to stop).map(pow(_, 2).toLong).sum
    val squareSum = pow((start to stop).sum, 2).toLong
    squareSum - sumSquares
  }
}