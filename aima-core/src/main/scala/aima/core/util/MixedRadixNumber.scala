/*
 * Copyright 2012, 2013 Alex DiCarlo
 *
 * This file is part of aima-scala.
 *
 * Aima-scala is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Aima-scala is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with aima-scala.  If not, see <http://www.gnu.org/licenses/>.
 */

package aima.core.util

import scala.annotation.tailrec

/**
 * For details on <a href="http://demonstrations.wolfram.com/MixedRadixNumberRepresentations/">
 *   Mixed Radix Number Representations.</a>
 *
 * @author Alex DiCarlo
 */
case class MixedRadixNumber(value: Long, radices: List[Int]) {self =>
  require(0 <= value && value <= maxAllowedValue, s"Invalid MRN: $self, 0 <= $value <= $maxAllowedValue is not true!")
  /**
   * Max allowed value for radices
   */
  lazy val maxAllowedValue: Long = (radices foldLeft 1) {case (max, radixValue) => max * radixValue} - 1
  /**
   * Returns value at position
   */
  lazy val numeralValue: List[Long] = (radices foldLeft(value, List[Long]()))
  {case ((quotient, values), radixValue) => (quotient / radixValue, values :+ (quotient % radixValue))}._2
  /**
   * @return MixedRadixNumber with value + 1
   */
  def inc(): MixedRadixNumber = MixedRadixNumber(value + 1, radices)
  /**
   * @return MixedRadixNumber with value - 1
   */
  def dec(): MixedRadixNumber = MixedRadixNumber(value - 1, radices)
}

object MixedRadixNumber {
  /**
   * Returns the value given the specified radices and values at each radix.
   *
   * @param radixValues value at each radix position
   * @param radices radix of each numeral position
   * @return base 10 value
   */
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
