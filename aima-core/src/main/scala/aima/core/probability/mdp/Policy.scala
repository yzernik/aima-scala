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

import scala.collection.MapProxy

trait Policy[S, A] {
  def action(state: S): A
}

case class MapPolicy[S, A](actionMap: Map[S, A]) extends Policy[S, A] with MapProxy[S, A] {
  def action(state: S): A = actionMap(state)
  def self = actionMap
}

object MapPolicy {
  def convert[S, A](policy: Policy[S, A], mdp: MarkovDecisionProcess[S, A]): MapPolicy[S, A] =
    MapPolicy(mdp.states.map(state => (state, policy.action(state)))(collection.breakOut))
}