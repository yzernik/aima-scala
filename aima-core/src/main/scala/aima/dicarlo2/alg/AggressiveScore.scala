package edu.uiuc.cs.dicarlo2.alg

object AggressiveScore {
  def apply(player: Player)(state: State): Int = {
    var playerScore = score(state)(player)
    for (i <- state.board.indices; j <- state.board.indices) {
      if (canBlitz(player)(i, j)(state.board))
        playerScore += state.board(i)(j).value
    }
    playerScore
  }

  def canBlitz(player: Player)(x: Int, y: Int)(board: Board): Boolean = {
    def canBlitz(adjX: Int, adjY: Int)(twoAwayX: Int, twoAwayY: Int): Boolean = {
      board.isValidPoint(twoAwayX, twoAwayY) && board(adjX)(adjY).player.isEmpty &&
        board(twoAwayX)(twoAwayY).player.exists(_ == player)
    }
    board(x)(y).player.isEmpty && (
      canBlitz(x + 1, y)(x + 2, y) || canBlitz(x + 1, y)(x + 1, y + 1) ||
        canBlitz(x, y + 1)(x, y + 2) || canBlitz(x, y + 1)(x - 1, y + 1) ||
        canBlitz(x - 1, y)(x - 2, y) || canBlitz(x - 1, y)(x - 1, y - 1) ||
        canBlitz(x, y - 1)(x, y - 2) || canBlitz(x, y - 1)(x + 1, y - 1))
  }
}
