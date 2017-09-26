package edu.caeus.rubik

import shapeless._

import scala.language.higherKinds
import scala.util.Random

/**
  * Created by undertrail on 23/06/17.
  */

object defs {
  type Vector3D = Int :: Int :: Int :: HNil
  type RMatrix = Vector3D :: Vector3D :: Vector3D :: HNil

  case class Piece(
                    face: Vector3D,
                    pos: Vector3D
                  )
  case class AimedPiece(curr:Piece,
                        aim:Piece)
  object AimedPiece{
    def apply(piece:Piece):AimedPiece={
      AimedPiece(curr = piece,aim = piece)
    }
    implicit class Ops(val piece: AimedPiece) extends AnyVal {
      def |>(movement: Movement): AimedPiece = {
        AimedPiece(piece.curr |> movement,piece.aim)
      }
    }
  }
  object Piece{
    implicit class Ops(val piece: Piece) extends AnyVal {
      def |>(movement: Movement): Piece = {
        if (movement.axis.extract(piece.pos) == movement.plane) {
          val matrix = movement.axis(movement.dir)
          piece.copy(face = matrix * piece.face,
            pos = matrix * piece.pos
          )
        } else piece
      }
    }
  }
  object Dir {
    implicit class Ops(val dir:Dir) extends AnyVal{
      def unary_! : Dir ={
        dir match {
          case `^*` => *^
          case `*^` => ^*
        }
      }
    }
    def random: Dir = {
      val random = new Random()
      random.nextInt(2) match {
        case 0 => ^*
        case 1 => *^
      }
    }
  }

  case class Movement(axis: Axis, dir: Dir, plane: Int)

  object Movement {
    def random = Movement(axis = Axis.random, dir = Dir.random, plane = {
      val random = new Random()
      random.nextInt(2) * 2 - 1
    })
    implicit class Ops(val movement: Movement) extends AnyVal{
      def unary_! : Movement ={
        Movement(axis = movement.axis,dir= !movement.dir,plane=movement.plane)
      }
    }
  }

  trait Monad[M[_]]{
    def bind[A,B](m:M[A],f:A=>M[B])
    def point[A](a:A):M[A]
  }





  implicit class Vector3DOps(val v1: Vector3D) extends AnyVal {
    def *(v2: Vector3D): Int = {
      v1.head * v2.head +
        v1.tail.head * v2.tail.head +
        v1.tail.tail.head * v2.tail.tail.head
    }

  }

  implicit class MultM(val value: Vector3D ::
    Vector3D ::
    Vector3D :: HNil) extends AnyVal {

    def *(vector: Vector3D): Int ::Int :: Int :: HNil = {
      value.head * vector ::
        value.tail.head * vector ::
        value.tail.tail.head * vector :: HNil
    }
  }

  implicit class PiecesOps(val cube:Seq[AimedPiece]){
    def |>(movement: Movement): Seq[AimedPiece] ={
      cube.map(_ |> movement)
    }
  }


  sealed trait Axis {
    def extract(position: Vector3D): Int

    def apply(dir: Dir): RMatrix
  }


  sealed trait Dir



  object ^* extends Dir

  object *^ extends Dir

  object X extends Axis {


    override def apply(dir: Dir): RMatrix = dir match {
      case ^* =>
        (1 :: 0 :: 0 :: HNil) ::
          (0 :: 0 :: -1 :: HNil) ::
          (0 :: 1 :: 0 :: HNil) :: HNil
      case *^ =>
        (1 :: 0 :: 0 :: HNil) ::
          (0 :: 0 :: 1 :: HNil) ::
          (0 :: -1 :: 0 :: HNil) :: HNil
    }

    override def extract(position: Vector3D): Int = position.head
  }

  object Y extends Axis {


    override def apply(dir: Dir): RMatrix = dir match {
      case `^*` =>
        (0 :: 0 :: 1 :: HNil) ::
          (0 :: 1 :: 0 :: HNil) ::
          (-1 :: 0 :: 0 :: HNil) :: HNil
      case `*^` =>
        (0 :: 0 :: -1 :: HNil) ::
          (0 :: 1 :: 0 :: HNil) ::
          (1 :: 0 :: 0 :: HNil) :: HNil
    }

    override def extract(position: Vector3D): Int = position.tail.head
  }

  object Z extends Axis {

    override def apply(dir: Dir): RMatrix = dir match {
      case `^*` =>
        (0 :: -1 :: 0 :: HNil) ::
          (1 :: 0 :: 0 :: HNil) ::
          (0 :: 0 :: 1 :: HNil) :: HNil
      case `*^` =>
        (0 :: 1 :: 0 :: HNil) ::
          (-1 :: 0 :: 0 :: HNil) ::
          (0 :: 0 :: 1 :: HNil) :: HNil
    }

    override def extract(position: Vector3D): Int = position.tail.tail.head
  }

  object Axis {
    def random: Axis = {
      val random = new Random()
      random.nextInt(3) match {
        case 0 => X
        case 1 => Y
        case 2 => Z
      }
    }
  }


}
