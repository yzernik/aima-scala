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

package aima.core.probability.learning

class QLearningAgent[S, A](actionsIn: S => Seq[A], gamma: Double, f: (Double, Int) => Double, alpha: Int => Double) {
  var Q = Map[(Option[S], Option[A]), Double]().withDefaultValue(0.0)
  var N = Map[(Option[S], Option[A]), Int]().withDefaultValue(0)
  var sPrime: Option[S] = None
  var aPrime: Option[A] = None
  var rPrime: Double = 0
  var t = 1

  def action(s: S, r: Double): Option[A] = {
    if (actionsIn(s).isEmpty) {
      N += ((Some(s), None) → (N((Some(s), None)) + 1))
      N += ((sPrime, aPrime) → (N((sPrime, aPrime)) + 1))
      Q += ((Some(s), None) → r)
      Q += ((sPrime, aPrime) →
        (Q((sPrime, aPrime)) + alpha(t) * (rPrime + gamma * r - Q((sPrime, aPrime)))))
      t += 1
      sPrime = None
      aPrime = None
      rPrime = 0
      aPrime
    } else {
      sPrime match {
        case Some(state) =>
          N += ((sPrime, aPrime) → (N((sPrime, aPrime)) + 1))
          val maxAPrime = (actionsIn(s) map {a => Q((Some(s), Some(a)))}).max
          Q += ((sPrime, aPrime) →
            (Q((sPrime, aPrime)) + alpha(t) * (rPrime + gamma * maxAPrime - Q((sPrime, aPrime)))))
        case None =>
      }
      sPrime = Some(s)
      rPrime = r
      aPrime = Some(
        actionsIn(s) maxBy {
          a => {
            val qValue = Q((Some(s), Some(a)))
            val nValue = N((Some(s), Some(a)))
            f(qValue, nValue)

          }
        })
      aPrime
    }
  }

  def utility(): Map[Option[S], Double] = {
    Q
      .groupBy({case ((state, action), value) => state})
      .map({case (state, map) => (state, map.map({case ((_, _), value) => value}).max)})
      .toMap
  }
}