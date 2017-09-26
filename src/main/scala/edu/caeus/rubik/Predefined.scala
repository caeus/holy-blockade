package edu.caeus.rubik

import edu.caeus.rubik.defs._
import shapeless._

/**
  * Created by undertrail on 23/06/17.
  */
object Predefined {
  def positions = Seq(
    -1 :: -1 :: -1 :: HNil,
    -1 :: -1 :: 0 :: HNil,
    -1 :: -1 :: 1 :: HNil,
    -1 :: 0 :: -1 :: HNil,
    -1 :: 0 :: 1 :: HNil,
    -1 :: 1 :: -1 :: HNil,
    -1 :: 1 :: 0 :: HNil,
    -1 :: 1 :: 1 :: HNil,
    0 :: -1 :: -1 :: HNil,
    0 :: -1 :: 1 :: HNil,
    0 :: 1 :: -1 :: HNil,
    0 :: 1 :: 1 :: HNil,
    1 :: -1 :: -1 :: HNil,
    1 :: -1 :: 0 :: HNil,
    1 :: -1 :: 1 :: HNil,
    1 :: 0 :: -1 :: HNil,
    1 :: 0 :: 1 :: HNil,
    1 :: 1 :: -1 :: HNil,
    1 :: 1 :: 0 :: HNil,
    1 :: 1 :: 1 :: HNil
  )
  def faces =Seq(
    1 :: 0 :: 0 :: HNil,
    -1 :: 0 :: 0 :: HNil,
    0 :: 1 :: 0 :: HNil,
    0 :: -1 :: 0 :: HNil,
    0 :: 0 :: 1 :: HNil,
    0 :: 0 :: -1 :: HNil
  )
  def dirs = Seq(^*,*^)

  def axes = Seq(X,Y,Z)

  def planes = Seq(1,-1)

  def movements: Seq[Movement] =for {
    axis <- axes
    dir <- dirs
    plane <- planes
  }yield{
    Movement(axis = axis,dir = dir,plane = plane)
  }
  def facing: Vector3D = 0 :: 0 :: 1 :: HNil


  def R3x3x3: Seq[AimedPiece] = {
    positions.map{
      position=>
        AimedPiece(Piece(face=facing,pos=position))
    }
  }
}
