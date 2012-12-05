package aima.core.search

/**
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/22/12
 */
package object adversarial {
  sealed case class Player(num: Int)
  type PlayerTurn[S] = S => Player
  type TerminalTest[S] = S => Boolean
  type Utility[S] = (S, Player) => Double
  type Cutoff[S] = (S, Int) => Boolean
  type AdversarialSearch[S, A] = S => SearchResult[A]
}
