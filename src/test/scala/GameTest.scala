package org.cc.game.go

import org.scalatest.FunSuite


class GameTest extends FunSuite {

  import Game._
  import Board._

  // An empty game
  val Start = Game()

// The SurroundedCrossPattern looks like this :
//
//  __| A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S
//  01| .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
//  02| .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
//  03| .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
//  04| .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
//  05| .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
//  06| .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
//  07| .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
//  08| .  .  .  .  .  .  .  .  .  o  .  .  .  .  .  .  .  .  .
//  09| .  .  .  .  .  .  .  .  o  *  o  .  .  .  .  .  .  .  .
//  10| .  .  .  .  .  .  .  o  *  .  *  o  .  .  .  .  .  .  .
//  11| .  .  .  .  .  .  .  .  o  *  o  .  .  .  .  .  .  .  .
//  12| .  .  .  .  .  .  .  .  .  o  .  .  .  .  .  .  .  .  .
//  13| .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
//  14| .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
//  15| .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
//  16| .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
//  17| .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
//  18| .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
//  19| .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .

  val SurroundedCrossPattern: List[Option[Position]] = List(
    pos(9,10),  // Black
    pos(8,10),  // White
    pos(11,10), // Black
    pos(9,9),   // White
    pos(10,9),  // Black
    pos(10,8),  // White
    pos(10,11), // Black
    pos(11,9),  // White
    pass(),     // Black
    pos(12,10), // White
    pass(),     // Black
    pos(11,11), // White
    pass(),     // Black
    pos(10,12), // White
    pass(),     // Black
    pos(9,11)   // White
  )

  test("Current player should alternate between Black and White at each turn") {
    // TODO
  }

  test("Passing turn should change the current player without changing the score or the board") {
    // TODO
  }

  test("Playing a position surrounded by the opposite color returns an error message") {
    val Right(game) = playAll(Start)(SurroundedCrossPattern)
    // This is Black to play
    assert(game.current==Black)
    // Black shouldn't be able to play (10,10) as the group is surrounded by White
    assert(play(game)(pos(10,10)).isLeft)
  }

  test("Playing a position that is taking opposite color pawns should score 1 point per Pawn taken") {
    val Right(game) = playAll(Start)(SurroundedCrossPattern ++ List(None))
    // This is White to play
    assert(game.current==White)
    assert(game.score.white==0)
    assert(game.score.black==0)
    // Should take 4 Black => White score should be +4
    val Right(next) = play(game)(pos(10,10))
    assert(next.score.white==4)
    assert(next.score.black==0)
  }


  test("Undoing a new game should not crash and return None") {
    assert(undo(Start).isEmpty)
  }

  test("Undoing a move should restore the game to the previous state") {
    val Right(game) = playAll(Start)(SurroundedCrossPattern ++ List(None))
    // This is White to play
    assert(game.current==White)
    assert(game.score.white==0)
    assert(game.score.black==0)
    // Should take 4 Black => White score should be +4
    val Right(next) = play(game)(pos(10,10))
    assert(next.score.white==4)
    assert(next.score.black==0)
    // Undoing the current game should restore the previous state / score, etc.
    val Some(undone) = undo(game)
    assert(game.current==White)
    assert(undone.score.white==0)
    assert(undone.score.black==0)
    assert(undone.board==game.board)
  }


  // FIXME : this test is failing
  test("Non-repetition rule : a game should not be able to come back to an immediate previous state") {

    // The RepetitionPattern looks like this :
    //
    //  __| A  B  C  D  E
    //  01| .  *  o  .  .
    //  02| *  .  .  o  .
    //  03| .  *  o  .  .
    //  04| .  .  .  .  .
    val RepetitionPattern: List[Option[Position]] = List(
      pos(2,1),  // Black
      pos(3,1),  // White
      pos(2,3),  // Black
      pos(3,3),  // White
      pos(1,2),  // Black
      pos(4,2)   // White
    )
    val Right(game) = playAll(Start)(RepetitionPattern)
    // This is Black to play
    assert(game.current==Black)
    // Should take 4 Black => White score should be +4
    val Right(next1) = play(game)(pos(3,2)) // Black
    val Right(next2) = play(next1)(pos(2,2)) // White is taking Black
    assert(next2.current==Black)
    assert(next2.score.white==1)
    // Black try to go back in (3,2) - this should be prevented
    assert(play(next2)(pos(3,2)).isLeft)
  }


  private def pos(x: Int, y: Int): Option[Position] = Some((x,y))
  private def pass() = None
}
