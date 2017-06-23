package edu.caeus.rubik

import edu.caeus.rubik.defs.Vector3D
import shapeless._

/**
  * Created by undertrail on 23/06/17.
  */
case class Piece(
                  facing: Vector3D,
                  position: Vector3D,
                  aim: Vector3D
                )


case class Pieces(value: Seq[Piece])


