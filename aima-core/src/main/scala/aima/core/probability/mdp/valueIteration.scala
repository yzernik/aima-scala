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

package aima.core.probability.mdp

import scala.annotation.tailrec

object valueIteration {
  def apply[S, A](mdp: MarkovDecisionProcess[S, A], gamma: Double, epsilon: Double): Map[S, Double] = {
    @tailrec
    def recur(utilities: Map[S, Double], delta: Double): Map[S, Double] = {
      if (delta < epsilon * (1 - gamma) / gamma) {
        utilities
      } else {
        val nextUtilities = mdp.states map {
          state => {
            val utility = (mdp rewardIn state) + gamma * (mdp actionsIn state map {
              action => mdp.states.foldLeft(0.0) {
                (sum, statePrime) => sum + mdp.transition(statePrime, state, action) * utilities(statePrime)
              }
            }).max
            (state, utility)
          }
        }
        val nextDelta = (nextUtilities map {case (state, util) => math.abs(util - utilities(state))}).max
        recur(nextUtilities.toMap, nextDelta)
      }
    }
    recur(Map().withDefault(s => 0.0), Double.MaxValue)
  }
}
