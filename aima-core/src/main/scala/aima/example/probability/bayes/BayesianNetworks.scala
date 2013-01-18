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

package aima.example.probability.bayes

import aima.core.probability.bayes.BayesianNetwork
import aima.core.probability.bayes.impl.{BayesNet, CPTNode}

object ToothCavityCatchNetwork {
  def apply(): BayesianNetwork = {
    lazy val cavity: CPTNode[Boolean] = CPTNode(RandomVariables.cavityRV, List(0.2, 0.8), List(), List(toothache,
      catchNode))
    lazy val toothache: CPTNode[Boolean] = CPTNode(RandomVariables.toothacheRV, List(
      // C=true, T=true
      0.6,
      // C=true, T=false
      0.4,
      // C=false, T=true
      0.1,
      // C=false, T=false
      0.9)
      , List[CPTNode[Boolean]](cavity), List())
    lazy val catchNode: CPTNode[Boolean] = CPTNode(RandomVariables.catchRV, List(
      // C=true, Catch=true
      0.9,
      // C=true, Catch=false
      0.1,
      // C=false, Catch=true
      0.2,
      // C=false, Catch=false
      0.8
    ), List(cavity), List())
    BayesNet(cavity)
  }
}