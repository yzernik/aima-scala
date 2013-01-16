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

package aima.core.search.online

import aima.core.search._

 /**
  * Artificial Intelligence A Modern Approach (3rd Edition): page 147.<br>
  * <br>
  * An online search problem must be solved by an agent executing actions, rather
  * than by pure computation. We assume a deterministic and fully observable
  * environment (Chapter 17 relaxes these assumptions), but we stipulate that the
  * agent knows only the following: <br>
  * <ul>
  * <li>ACTIONS(s), which returns a list of actions allowed in state s;</li>
  * <li>The step-cost function c(s, a, s') - note that this cannot be used until
  * the agent knows that s' is the outcome; and</li>
  * <li>GOAL-TEST(s).</li>
  * </ul>
  *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/20/12
 */
case class OnlineSearchProblem[S, A](
  initialState: S,
  actions: Actions[S, A],
  goalTest: GoalTest[S],
  stepCost: StepCost[S, A])
