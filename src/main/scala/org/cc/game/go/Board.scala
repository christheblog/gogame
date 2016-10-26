package org.cc.game.go

import scala.annotation.tailrec
import scala.collection.immutable.{Queue, Set}


object Board {

  sealed trait Color
  case object Black extends Color
  case object White extends Color

  object Color {
    def format(c: Color) = c match {
      case Black => "Black"
      case White => "White"
    }
    def symbol(c: Color) = c match {
      case Black => "*"
      case White => "o"
    }
  }

  type Position = (Int,Int)
  type Chain = Set[Position]


  case class Board(n: Int = 19, pawns: Map[Position,Color] = Map())


  def colorAt(board: Board)(pos: Position) =
    board.pawns.get(pos)

  def isFree(board: Board)(pos: Position) =
    colorAt(board)(pos).isEmpty

  def isBusy(board: Board)(pos: Position) =
    !isFree(board)(pos)

  def isOnBoard(board: Board)(pos: Position) = {
    val (x,y) = pos
    x > 0 && x <= board.n && y > 0 && y <= board.n
  }

  def opposite(color: Color) = color match {
    case Black => White
    case White => Black
  }

  // Neighbours are North, South, West and East positions that are on the board
  // Diagonals are not considered
  def neighbours(board: Board)(pos: Position): List[Position] = {
    val (x,y) = pos
    List((x,y-1),(x-1,y),(x,y+1),(x+1,y)).filter(isOnBoard(board))
  }

  // Check if the color of the chain containing the start position is surrounded by the opposite color
  def isChainSurrounded(board: Board)(start: Position): Boolean =
    (getSurroundedChain(board)(start)).isDefined

  // Returns Some(chain) if the color of the chain containing the start position is surrounded by the opposite color
  def getSurroundedChain(board: Board)(start: Position): Option[Chain] = {
    val colorfn = colorAt(board) _
    val neighbfn = neighbours(board) _
    colorfn(start) match {
      case chain@Some(color) =>
        val opp = opposite(color)
        @tailrec def bfs(toProcess: Queue[Position], visited: Set[Position]): Option[Chain] =
          toProcess match {
            case x +: xs =>
              val (sameColor, oppositeOrEmpty) = neighbfn(x)
                .map { case pos => (pos, colorfn(pos)) }
                .partition(_._2==chain)
              // An empty position means the chain is not surrounded
              if(oppositeOrEmpty.exists(_._2.isEmpty)) None
              else {
                val candidates = sameColor.map(_._1)
                bfs(xs ++ candidates.filterNot(visited), visited ++ candidates)
              }
            // No more candidates and no empty position found
            // This means all neighbours of the chain are of the opposite color
            // => we are surrounded
            case _ => Some(visited)
          }
        bfs(Queue(start), Set(start))
      case _ => None
    }
  }

  // Formatting the Board
  def format(board: Board): String = {
    val header = ("__|" +: ('A' until ('A'.toInt + board.n).toChar)
      .map(x => s" ${x} "))
      .mkString("","","\n")
    val display =
      (for(y <- 1 to board.n; x <- 1 to board.n) yield {
        board.pawns.get((x,y))
          .map { c => s" ${Color.symbol(c)} " }
          .getOrElse(" . ")
      }).grouped(board.n)
        .zipWithIndex
        .map{ case (row,i) => row.mkString("%02d|".format(i+1),"","\n") }
        .mkString("","","\n")
    header + display
  }

}
