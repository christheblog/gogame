package org.cc.game.go

import scala.annotation.tailrec
import scala.io.StdIn


object GoGame extends App {


  import Game._
  import Board._


  // REPL loop taking commands for the game
  @tailrec
  def repl(game: Game): Unit = {

    def separator(): Unit = println("-"*80)
    def feedback(msg: String): Unit = {
      println(msg)
      separator()
    }

    // Parsing a position given by an absciss letter and a row number : A1, D12, R19, ...
    val PositionRE = "^([A-S])([0-9]{1,2})$".r
    def parsePosition(input: String): Either[String,Position] = input.trim match {
      case PositionRE(x,y) if y.toInt > 0 && y.toInt < 20 =>
        Right(((x.charAt(0).toInt - 'A'.toInt)+1,y.toInt))
      case _ =>
        Left(s"Invalid coordinates : ${input}")
    }

    println(s"Black: ${game.score.black}, White: ${game.score.white}")
    println(format(game.board))
    separator()

    val input = StdIn.readLine(s"${Color.format(game.current)} player's turn : ")

    input.trim match {
      // Quit the application
      case "q" | "quit" => // nothing to do here
      // Restarting the game
      case "restart" =>
        println(s"Restarting game ...")
        repl(Game())
      // Compute scores
      case "end" =>
        println(s"Final score : Black: ${game.score.black}, White: Black: ${game.score.white}")
      case "undo" =>
        println(s"Undoing last move ...")
        undo(game) match {
          case None => println("Unable to undo last move !"); repl(game)
          case Some(prev) => repl(prev)
        }
      // Passing turn
      case "pass" =>
        pass(game) match {
          case Left(msg) => feedback(msg); repl(game)
          case Right(next) => repl(next)
        }
      // None of the above, we try to interpret the command as coordinates for the next move
      case str => parsePosition(str) match {
        case Right(pos) =>
          play(game)(Some(pos)) match {
            case Left(msg) => feedback(msg); repl(game)
            case Right(next) => repl(next)
          }
        case Left(msg) =>
          feedback(msg)
          repl(game)
      }
    }
  }

  println(s" ********                      ********")
  println(s" ******** Welcome to GO v0.0.1 ********")
  println(s" ********                      ********")
  println("Staring game, Black player first ...")
  repl(Game())
}
