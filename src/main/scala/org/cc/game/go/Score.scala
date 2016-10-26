package org.cc.game.go

// TODO : add territory counting + Chinese/Japanese rules
object Score {

  import Board._

  case class Score(black: Int = 0,
                   white: Int = 0,
                   handicap: Int = 13)

  def increase(score: Score)(color: Color)(points: Int) = color match {
    case Black => Score(black = score.black + points)
    case White => Score(white = score.white + points)
  }

  def winner(score: Score): Option[Color] = {
    import score._
    if(black > white + handicap) Some(Black)
    else if (black <= white + handicap) Some(White)
    else None
  }

}
