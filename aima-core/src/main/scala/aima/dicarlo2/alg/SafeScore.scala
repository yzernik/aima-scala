package edu.uiuc.cs.dicarlo2.alg

object SafeScore {
  def apply(player: Player)(state: State): Int = {
    var playerScore = score(state)(player)
    for (i <- state.board.indices; j <- state.board.indices) {
      if (enemyCanBlitz(player)(i, j)(state.board))
        playerScore -= state.board(i)(j).value / 2
    }
    playerScore
  }

  def enemyCanBlitz(player: Player)(x: Int, y: Int)(board: Board): Boolean = {
    val enemy = nextPlayer(player)
    def canBlitz(adjX: Int, adjY: Int)(twoAwayX: Int, twoAwayY: Int): Boolean = {
      board.isValidPoint(twoAwayX, twoAwayY) && board(adjX)(adjY).player.isEmpty &&
        board(twoAwayX)(twoAwayY).player.exists(_ == enemy)
    }
    board(x)(y).player.exists(_ == player) && (
      canBlitz(x + 1, y)(x + 2, y) || canBlitz(x + 1, y)(x + 1, y + 1) ||
        canBlitz(x, y + 1)(x, y + 2) || canBlitz(x, y + 1)(x - 1, y + 1) ||
        canBlitz(x - 1, y)(x - 2, y) || canBlitz(x - 1, y)(x - 1, y - 1) ||
        canBlitz(x, y - 1)(x, y - 2) || canBlitz(x, y - 1)(x + 1, y - 1))
  }
}
