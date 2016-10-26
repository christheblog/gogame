package org.cc.game.go


object History {

  import Board._

  type Move = Option[Position]

  case class History(prevBlack: Option[Board] = None,
                     prevWhite: Option[Board] = None,
                     history: List[(Move,Color)] = Nil)

  def isEmpty(h: History) =
    h.history.isEmpty

  // Register the move
  def register(h: History)(board: Board)(color: Color)(move: Move): History =
    color match {
      case Black => h.copy(prevBlack = Some(board), history = (move,color) :: h.history)
      case White => h.copy(prevWhite = Some(board), history = (move,color) :: h.history)
    }
}
