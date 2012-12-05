package edu.uiuc.cs.dicarlo2

import edu.uiuc.cs.dicarlo2.alg._

object Printer {
  def printBoard(board: Board) {
    for (j ← board(0).indices) {
      for (i ← board.indices) {
        print(board(i)(j) + " ")
      }
      println()
    }
  }

  implicit def space2string(space: Space): String = space match {
    case Space(value, Some(player)) => f"(${player.num}):$value%2d"
    case Space(value, _) => f"   :$value%2d"
  }
}
