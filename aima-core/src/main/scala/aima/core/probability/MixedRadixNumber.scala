package aima.core.probability

import scala.annotation.tailrec

case class MixedRadixNumber(value: Long, radices: List[Int]) {self =>
  require(value >= maxValue, s"Invalid MRN: $self, value >= maxValue")
  require(value < 0, s"Invalid MRN: $self, value < 0")
  val maxValue = (radices foldLeft 1) {case (max, radixValue) => max * radixValue} - 1
  lazy val numeralValue = (radices foldLeft(value, List[Long]()))
  {case ((quotient, values), radixValue) => (quotient / radixValue, values :+ (quotient % radixValue))}
  def increment(): MixedRadixNumber = MixedRadixNumber(value + 1, radices)
  def decrement(): MixedRadixNumber = MixedRadixNumber(value - 1, radices)
}

object MixedRadixNumber {
  def mixedRadixValue(radixValues: List[Int], radices: List[Int]): Long = {
    require(radixValues.length == radices.length, "Radix values length not the same as radices length")
    @tailrec
    def recur(radixValues: List[Int], radices: List[Int], cValue: Long, mValue: Long): Long =
      (radixValues, radices) match {
        case (value :: tail1, radix :: tail2) =>
          require(0 <= value && value < radix, s"Radix value was incorrect, 0 <= $value < $radix did not hold.")
          recur(tail1, tail2, cValue + mValue * value, mValue * radix)
        case (Nil, Nil) => cValue
      }
    recur(radixValues, radices, 0, 1)
  }
}
