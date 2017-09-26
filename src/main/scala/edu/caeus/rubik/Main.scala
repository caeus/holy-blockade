package edu.caeus.rubik

import edu.caeus.rubik.defs._
import edu.caeus.rubik.distances.piecesfw.PieceFW

import scala.annotation.tailrec
import scala.collection.immutable

/**
  * Created by undertrail on 23/06/17.
  */
object Main {
  @tailrec
  def solve(cube: Seq[AimedPiece], dist: Seq[AimedPiece] => Double, solution: List[Movement] = Nil): List[Movement] = {
    if (solution.length > 500) throw new Exception("it takes so fucking long")
    dist(cube) match {
      case 0 => solution.reverse
      case currentDistance => val (newMove, newCube, newDistance) = Predefined.movements.map {
        movement =>
          val pieces = cube |> movement
          (movement, pieces, dist(pieces))
      }.minBy {
        case (_, _, dist) => dist
      }
        if (newDistance >= currentDistance) throw new Exception("Your algorithm sucks")
        solve(newCube, dist, newMove :: solution)
    }
  }


  def main(args: Array[String]): Unit = {
    val scramble: immutable.Seq[Movement] = (0 until 100).map { _ =>
      Movement.random
    }
    val unscramble = scramble.map(!_).reverse

    val scrambledCube = scramble.foldLeft(Predefined.R3x3x3) {
      (cube, move) =>
        cube |> move
    }
    val reassembledCube = unscramble.foldLeft(scrambledCube) {
      (cube, move) =>

        val pieces = cube |> move
        println(PieceFW(pieces))
        pieces
    }
    solve(scrambledCube, PieceFW)

    println(reassembledCube == Predefined.R3x3x3)
    println(scrambledCube == Predefined.R3x3x3)
  }
}
