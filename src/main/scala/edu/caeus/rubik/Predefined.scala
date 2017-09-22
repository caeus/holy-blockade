package edu.caeus.rubik

import edu.caeus.rubik.defs._
import shapeless._

/**
  * Created by undertrail on 23/06/17.
  */
object Predefined {
  private def facing: Vector3D = 0 :: 0 :: 1 :: HNil

  def R3x3x3: Seq[Piece] = {
    Seq(
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
    ).map{
      position=>
        Piece(facing=facing,position=position,aim=position)
    }
  }
}
