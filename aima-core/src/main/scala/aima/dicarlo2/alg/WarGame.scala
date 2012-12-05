package edu.uiuc.cs.dicarlo2.alg

sealed abstract class Player {def num: Int}

case object Player1 extends Player {override def num: Int = 1}

case object Player2 extends Player {override def num: Int = 2}

sealed case class Space(value: Int, player: Option[Player])

sealed case class State(board: Board, turn: Player)

sealed abstract class Action

case class CommandoParaDrop(x: Int, y: Int, player: Player) extends Action

case class M1DeathBlitz(x: Int, y: Int, player: Player) extends Action

sealed case class Board(private val board: IndexedSeq[IndexedSeq[Space]]) {
  def apply(i: Int): IndexedSeq[Space] = board(i)

  def isValidPoint(x: Int, y: Int): Boolean = x < board.size && x >= 0 && y < board(0).size && y >= 0

  def canMove(player: Player)(fromX: Int, fromY: Int)(toX: Int, toY: Int): Boolean = {
    def isValidMove(fromX: Int, fromY: Int)(toX: Int, toY: Int) = {
      def valid1DirectionMove: (Int, Int) => Boolean = (from, to) => from == to - 1 || from == to + 1
      (fromX == toX && valid1DirectionMove(fromY, toY)) || (fromY == toY && valid1DirectionMove(fromX, toX))
    }
    isValidPoint(fromX, fromY) && board(fromX)(fromY).player.exists(_ == player) &&
      isValidPoint(toX, toY) && board(toX)(toY).player.isEmpty && isValidMove(fromX, fromY)(toX, toY)
  }
}

object Board {
  implicit def indexedSeqIndexedSeqSpace2board(board: IndexedSeq[IndexedSeq[Space]]): Board = Board(board)

  implicit def board2indexedSeqIndexedSeqSpace(board: Board): IndexedSeq[IndexedSeq[Space]] = board.board

  def capture(player: Player)(board: Board, x: Int, y: Int): Board = {
    board.updated(x, board(x).updated(y, Space(board(x)(y).value, Some(player))))
  }
}
