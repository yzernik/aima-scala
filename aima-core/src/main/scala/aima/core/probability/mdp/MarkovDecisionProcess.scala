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

trait MarkovDecisionProcess[S, A] {
  def initialState: S
  def states: Seq[S]
  def actionsIn(state: S): Seq[A]
  def transition(statePrime: S, state: S, action: A): Double
  def rewardIn(state: S): Double
}

object MarkovDecisionProcess {
  type Actions[S, A] = S => Seq[A]
  type TransitionModel[S, A] = (S, S, A) => Double
  type Reward[S] = S => Double

  def apply[S, A](
    aInitialState: S,
    aStates: Seq[S],
    aActions: Actions[S, A],
    aTransitionModel: TransitionModel[S, A],
    aReward: Reward[S]): MarkovDecisionProcess[S, A] = new MarkovDecisionProcess[S, A] {
    val initialState: S = aInitialState
    val states: Seq[S] = aStates
    def actionsIn(state: S): Seq[A] = aActions(state)
    def transition(statePrime: S, state: S, action: A): Double = aTransitionModel(statePrime, state, action)
    def rewardIn(state: S): Double = aReward(state)
  }
}


