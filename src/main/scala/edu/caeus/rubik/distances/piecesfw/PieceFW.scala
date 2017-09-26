package edu.caeus.rubik.distances.piecesfw

import edu.caeus.rubik.Predefined
import edu.caeus.rubik.defs._

object PieceFW extends ((Seq[AimedPiece]) => Double) {


  private object positions {
    val all: Seq[Piece] = for {
      face <- Predefined.faces
      position <- Predefined.positions
    } yield Piece(face = face, pos = position)
  }

  private val distance = {
    val distance = scala.collection.mutable.Map[AimedPiece, Double]()
    positions.all.foreach {
      piece => distance(AimedPiece(piece)) = 0.0
    }


    for {
      piece <- positions.all
      movement <- Predefined.movements
    }
    //This looks weird, but they work together
    {
      val newP = piece |> movement
      if (newP != piece) {

        distance(AimedPiece(newP, piece)) = 1.0
        distance(AimedPiece(piece, newP)) = 1.0
      }
    }


    for {
      through <- positions.all
      from <- positions.all
      to <- positions.all
      newD = distance.getOrElse(AimedPiece(from , through), Double.PositiveInfinity) +
        distance.getOrElse(AimedPiece(through,to), Double.PositiveInfinity)
    } {
      if (newD <
        distance.getOrElse(AimedPiece(from, to), Double.PositiveInfinity)
      ) distance(AimedPiece(from, to))= newD
    }

    distance.toMap
  }

  override def apply(v1: Seq[AimedPiece]): Double = {
    v1.map {
      piece =>
        distance(piece)+1
    }.product
  }
}
