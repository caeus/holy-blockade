package edu.caeus.rubik

import edu.caeus.rubik.defs.{Axis, Dir}


/**
  * Created by undertrail on 23/06/17.
  */
case class Movement(axis: Axis, dir: Dir, plane: Int) {

  def apply(piece: Piece) = {
    if (axis.extract(piece.position) == plane) {
      val matrix = dir.matrix(axis)
      piece.copy(facing = matrix * piece.facing,
        position = matrix * piece.position
      )
    } else piece
  }


}
