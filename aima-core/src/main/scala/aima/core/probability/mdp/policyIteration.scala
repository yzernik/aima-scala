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

package aima.core.probability.mdp

import scala.util.Random.shuffle
import scala.annotation.tailrec

object policyIteration {
  type PolicyEvaluation[S, A] = (Map[S, A], Map[S, Double], MarkovDecisionProcess[S, A]) => Map[S, Double]

  def apply[S, A](policyEval: PolicyEvaluation[S, A])(mdp: MarkovDecisionProcess[S, A]): Policy[S, A] = {
    @tailrec
    def recur(utilities: Map[S, Double], states: List[S], policy: Map[S, A], changed: Boolean): Policy[S, A] =
      states match {
        case state :: tail =>
          def sum(s: S, a: A): Double =
            (mdp.states map {sPrime => mdp.transition(sPrime, s, a) * utilities(sPrime)}).sum
          val maxAction = mdp actionsIn state maxBy {sum(state, _)}
          if (sum(state, maxAction) > sum(state, policy(state)))
            recur(utilities, tail, policy + (state → maxAction), changed = true)
          else
            recur(utilities, tail, policy, changed)
        case Nil if changed => recur(policyEval(policy, utilities, mdp), mdp.states.toList, policy, changed = false)
        case Nil => MapPolicy(policy)
      }
    val initialPolicy = (mdp.states map {state => (state, shuffle(mdp actionsIn state).head)}).toMap
    recur(Map().withDefault(s => 0.0), mdp.states.toList, initialPolicy, changed = true)
  }

  def apply[S, A](k: Int, gamma: Double)(mdp: MarkovDecisionProcess[S, A]): Policy[S, A] = {
    val policyEvaluation: PolicyEvaluation[S, A] = (policy, utilities, mdp) => {
      @tailrec
      def recur(times: List[Int], states: List[S], utilities: Map[S, Double], next: Map[S, Double]): Map[S, Double] =
        (times, states) match {
          case (_, state :: tail) =>
            val sum = (mdp.states map {sPrime => mdp.transition(sPrime, state, policy(state)) * utilities(sPrime)}).sum
            recur(times, tail, utilities, next + (state → sum))
          case (_ :: tail, Nil) => recur(tail, mdp.states.toList, next, Map())
          case (Nil, Nil) => next
        }
      recur((0 to k).toList, mdp.states.toList, utilities, Map())
    }
    policyIteration(policyEvaluation)(mdp)
  }
}
