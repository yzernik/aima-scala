/*
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

object MathUtils {
  final class Precision(val p: Double) extends AnyVal

  implicit val defaultPrecision: Precision = new Precision(1e-5)

  implicit class DoubleEqualsPrecision(val d: Double) extends AnyVal {
    @inline def leq(d2: Double)(implicit p: Precision): Boolean = d < d2 || ~=(d2)
    @inline def geq(d2: Double)(implicit p: Precision): Boolean = d > d2 || ~=(d2)
    @inline def ~=(d2: Double)(implicit p: Precision): Boolean = (d - d2).abs < p.p
    @inline def !~=(d2: Double)(implicit p: Precision): Boolean = !(~=(d2))(p)
  }
  implicit class FloatEqualsPrecision(val d: Float) extends AnyVal {
    @inline def leq(d2: Float)(implicit p: Precision): Boolean = d < d2 || ~=(d2)
    @inline def geq(d2: Float)(implicit p: Precision): Boolean = d > d2 || ~=(d2)
    @inline def ~=(d2: Float)(implicit p: Precision): Boolean = (d - d2).abs < p.p
    @inline def !~=(d2: Float)(implicit p: Precision): Boolean = !(~=(d2))(p)
  }
  @inline def within(a: Double, b: Double, c: Double): Boolean = ((a leq b) && (b leq c)) || ((a geq b) && (b geq c))
}
