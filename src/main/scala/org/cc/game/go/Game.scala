package org.cc.game.go


object Game {

  import Board._
  import Score._
  import History._

  case class Game(current: Color = Black,
                  board: Board = Board(),
                  score: Score = Score(),
                  history: History = History())

  def create() = Game()


  // Playing commands

  def play(game: Game)(pos: Move): Either[String,Game] = pos match {
    case Some(pos@(x,y)) =>

      def add(board: Board)(color: Color)(pos: Position) =
        board.copy(pawns = board.pawns + (pos->color))
      def remove(board: Board)(positions: Chain) =
        positions.foldLeft(board) { case (b,p@(x,y)) => b.copy(pawns = b.pawns - p)}
      // FXIME : repetion check is not working at the moment
      def isRepetition(prev: Option[Board])(board: Board) =
        Some(board)==prev

      val board = game.board
      val color = game.current
      val previous = color match {
        case Black => game.history.prevBlack
        case White => game.history.prevWhite
      }
      val newBoard = add(board)(color)(pos)

      if(!isOnBoard(board)(pos))
        Left(s"Position ($x,$y) is not on the board")
      else if(isBusy(board)(pos))
        Left(s"Position ($x,$y) is busy")
      else if(isRepetition(previous)(newBoard))
        Left(s"Impossible to play this position ($x,$y) at that turn.")
      else {
        // Checking the opposite color neighbours of the position
        // If they belongs to surrounded chains => we need to remove the chains
        // else we don't do anything
        val toCheck = neighbours(newBoard)(pos)
          .map( p=> (p,colorAt(newBoard)(p)))
          .collect { case (p,Some(c)) if opposite(c)==color => p }
        val taken = toCheck
          .map { p => getSurroundedChain(newBoard)(p) }
          .collect { case Some(chain) => chain }
          .flatten.toSet
        // If nothing is taken, we need to double-check
        // that we are not creating a surrounded chain ourselves
        // In which case we cannot play to that position !
        if(taken.isEmpty && getSurroundedChain(newBoard)(pos).isDefined)
          Left(s"Impossible to play at position ($x,$y) - as it would give points to your opponent.")
        else {
          val finalBoard = remove(newBoard)(taken)
          Right(game.copy(current = opposite(color),
            board = finalBoard,
            score = increase(game.score)(color)(taken.size),
            history = register(game.history)(board)(game.current)(Some(pos))))
        }
      }
    // Current user is passing its turn
    case None =>
      Right(game.copy(current=opposite(game.current),
                      history = register(game.history)(game.board)(game.current)(None)))
  }

  // Play several positions the one after the others and returns the game state if all moves are successful
  // Else returns the description of the first error encountered
  def playAll(game: Game)(positions: List[Move]): Either[String,Game] =
    positions.foldLeft(Right(game): Either[String,Game]) {
      case (Right(g),pos) => play(g)(pos)
      case (Left(msg),_) => Left(msg)
    }

  def pass(game: Game): Either[String,Game] =
    play(game)(None)

  def end(game: Game): Score =
    // FIXME : compute territories, apply handicap, etc.
    // No idea of how this work in go, I should really read the rules at some point :)
    // But it is here it should be done
    game.score

  // Replay the history on a new game
  def replay(history: List[(Move,Color)]): Either[String,Game] =
    playAll(Game())(history.map(_._1).reverse)

  // Undo a move by reapplying the history of moves without the last one
  def undo(game: Game): Option[Game] =
    if(isEmpty(game.history)) None
    else replay(game.history.history.tail) match {
      case Left(msg) => None
      case Right(replayed) => Some(replayed)
    }

}
