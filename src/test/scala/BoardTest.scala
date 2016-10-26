package org.cc.game.go

import org.scalatest.FunSuite


class BoardTest extends FunSuite {

  import Board._

  val EmptyBoard = Board()

  test("A position in the middle of the board should have 4 neighbours") {
    val adjacents = neighbours(EmptyBoard)((10,10))
    assert(adjacents.size==4)
    assert(adjacents.contains((10,9)))
    assert(adjacents.contains((10,11)))
    assert(adjacents.contains((9,10)))
    assert(adjacents.contains((11,10)))
  }

  test("A position in a corner of the board should have only 2 neighbours") {
    val adjacents = neighbours(EmptyBoard)((1,1))
    assert(adjacents.size==2)
    assert(adjacents.contains((1,2)))
    assert(adjacents.contains((2,1)))
  }

  test("A position in the middle of a border should have only 3 neighbours") {
    val adjacents = neighbours(EmptyBoard)((10,1))
    assert(adjacents.size==3)
    assert(adjacents.contains((10,2)))
    assert(adjacents.contains((11,1)))
    assert(adjacents.contains((9,1)))
  }

  test("A Black position with only 3 of N,S,E,W White positions is NOT surrounded") {
    val BlackPosition = pos(10,10,Black)
    val board = putAll(EmptyBoard)(List(BlackPosition, pos(10,9,White) /*, pos(10,11,White) */, pos(9,10,White),pos(11,10,White)))
    val chain = getSurroundedChain(board)((10,10))
    assert(chain.isEmpty)
  }

  test("A Black position with N,S,E,W White positions is surrounded") {
    val BlackPosition = pos(10,10,Black)
    val board = putAll(EmptyBoard)(List(BlackPosition, pos(10,9,White), pos(10,11,White),pos(9,10,White),pos(11,10,White)))
    val Some(chain) = getSurroundedChain(board)((10,10))
    assert(chain.size==1)
    assert(chain((10,10)))
  }

  test("A Black position in a corner with only 2 of N,S,E,W White positions is surrounded") {
    val BlackPosition = pos(1,1,Black)
    val board = putAll(EmptyBoard)(List(BlackPosition, pos(1,2,White), pos(2,1,White)))
    val Some(chain) = getSurroundedChain(board)((1,1))
    assert(chain.size==1)
    assert(chain((1,1)))
  }

  test("A Black cross-shaped chain can be surrounded by White") {
    val Center = pos(10,10,Black)
    val BlackChain = List(Center, pos(9,10,Black), pos(11,10,Black), pos(10,9,Black), pos(10,11,Black))
    val WhiteSurrounding = List(pos(8,10,White), pos(9,9,White), pos(10,8,White), pos(11,9,White),
                                pos(12,10,White), pos(11,11,White), pos(10,12,White), pos(9,11,White))
    val board = putAll(EmptyBoard)(BlackChain ++ WhiteSurrounding)
    BlackChain.foreach { case (pos, _) =>
      val Some(chain) = getSurroundedChain(board)(pos)
      assert(chain.size == BlackChain.size)
    }
  }

  test("A Black cross-shaped chain with a missing center CANNOT be surrounded by White") {
    val BlackChain = List(pos(9,10,Black), pos(11,10,Black), pos(10,9,Black), pos(10,11,Black))
    val WhiteSurrounding = List(pos(8,10,White), pos(9,9,White), pos(10,8,White), pos(11,9,White),
      pos(12,10,White), pos(11,11,White), pos(10,12,White), pos(9,11,White))
    val board = putAll(EmptyBoard)(BlackChain ++ WhiteSurrounding)
    BlackChain.foreach { case (pos, _) =>
      assert((getSurroundedChain(board)(pos)).isEmpty)
    }
  }

  test("A Black cross-shaped chain with a White center results in 4 chains of 1 element being surrounded by White") {
    val Center = pos(10,10,White)
    val BlackChain = List(pos(9,10,Black), pos(11,10,Black), pos(10,9,Black), pos(10,11,Black))
    val WhiteSurrounding = List(Center,
      pos(8,10,White), pos(9,9,White), pos(10,8,White), pos(11,9,White),
      pos(12,10,White), pos(11,11,White), pos(10,12,White), pos(9,11,White))
    val board = putAll(EmptyBoard)(BlackChain ++ WhiteSurrounding)
    BlackChain.foreach { case (pos, _) =>
      val Some(chain) = getSurroundedChain(board)(pos)
      assert(chain.size == 1)
    }
  }

  // Helper

  private def put(board: Board)(p: Position)(color: Color): Board =
    board.copy(pawns = board.pawns + (p-> color))

  private def putAll(board: Board)(positions: List[(Position,Color)]): Board =
    positions.foldLeft(board) { case (brd,(p,c)) => put(brd)(p)(c) }

  private def pos(x: Int, y: Int, c: Color) = ((x,y),c)

}
