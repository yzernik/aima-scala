package edu.uiuc.cs.dicarlo2

import edu.uiuc.cs.dicarlo2.alg.Board.capture

package object alg {
  type Cutoff = (State, Int) => Boolean
  type Utility = State => Int

  def nextPlayer(player: Player): Player = player match {
    case Player1 => Player2
    case Player2 => Player1
  }

  def terminalTest(state: State): Boolean = state.board.forall(_.forall(space => space.player.isDefined))

  def score(state: State)(player: Player): Int = {
    state.board.map(_.filter(_.player.exists(_ == player)).map(_.value).sum).sum
  }

  def actions(state: State): Seq[Action] = {
    val board = state.board
    var actions = Seq[Action]()

    for (x <- 0 until board.size; y <- 0 until board(x).size; canMvTo = board.canMove(state.turn)(x, y) _;
      cantMvFrom = !board.canMove(state.turn)(_: Int, _: Int)(x, y)) {
      board(x)(y).player match {
        case None if (cantMvFrom(x + 1, y) && cantMvFrom(x, y + 1) && cantMvFrom(x - 1, y) && cantMvFrom(x, y - 1)) =>
          actions :+= CommandoParaDrop(x, y, state.turn)
        case Some(somePlayer) if somePlayer == state.turn => {
          if (canMvTo(x + 1, y)) actions :+= M1DeathBlitz(x + 1, y, state.turn)
          if (canMvTo(x, y + 1)) actions :+= M1DeathBlitz(x, y + 1, state.turn)
          if (canMvTo(x - 1, y)) actions :+= M1DeathBlitz(x - 1, y, state.turn)
          if (canMvTo(x, y - 1)) actions :+= M1DeathBlitz(x, y - 1, state.turn)
        }
        case _ =>
      }
    }
    actions
  }

  def result(state: State, action: Action): State = action match {
    case CommandoParaDrop(x, y, player) => State(capture(player)(state.board, x, y), nextPlayer(state.turn))
    case M1DeathBlitz(x, y, player) => {
      var board = capture(player)(state.board, x, y)
      def attemptCapture(x: Int, y: Int) {
        if (board.isValidPoint(x, y) && board(x)(y).player.exists(_ != player)) board = capture(player)(board, x, y)
      }

      attemptCapture(x + 1, y)
      attemptCapture(x, y + 1)
      attemptCapture(x - 1, y)
      attemptCapture(x, y - 1)

      State(board, nextPlayer(state.turn))
    }
  }
}
