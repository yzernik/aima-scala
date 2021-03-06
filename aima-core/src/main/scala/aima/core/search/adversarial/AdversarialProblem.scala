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

package aima.core.search.adversarial

import aima.core.search._

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): page 165.<br>
 * <br>
 * A game can be formally defined as a kind of search problem with the following
 * elements: <br>
 * <ul>
 * <li>S0: The initial state, which specifies how the game is set up at the
 * start.</li>
 * <li>PLAYER(s): Defines which player has the move in a state.</li>
 * <li>ACTIONS(s): Returns the set of legal moves in a state.</li>
 * <li>RESULT(s, a): The transition model, which defines the result of a move.</li>
 * <li>TERMINAL-TEST(s): A terminal test, which is true when the game is over
 * and false TERMINAL STATES otherwise. States where the game has ended are
 * called terminal states.</li>
 * <li>UTILITY(s, p): A utility function (also called an objective function or
 * payoff function), defines the final numeric value for a game that ends in
 * terminal state s for a player p. In chess, the outcome is a win, loss, or
 * draw, with values +1, 0, or 1/2 . Some games have a wider variety of possible
 * outcomes; the payoffs in backgammon range from 0 to +192. A zero-sum game is
 * (confusingly) defined as one where the total payoff to all players is the
 * same for every instance of the game. Chess is zero-sum because every game has
 * payoff of either 0 + 1, 1 + 0 or 1/2 + 1/2 . "Constant-sum" would have been a
 * better term, but zero-sum is traditional and makes sense if you imagine each
 * player is charged an entry fee of 1/2.</li>
 * </ul>
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/22/12
 */
case class AdversarialProblem[S, A](
  initialState: S,
  playerTurn: PlayerTurn[S],
  actions: Actions[S, A],
  result: Result[S, A],
  terminalTest: TerminalTest[S],
  utility: Utility[S])
