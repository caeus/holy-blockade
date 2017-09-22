package edu.caeus.rubik

import shapeless._

import scala.util.Random

/**
  * Created by undertrail on 23/06/17.
  */

object defs {
  type Vector3D = Int :: Int :: Int :: HNil
  type RMatrix = Vector3D :: Vector3D :: Vector3D :: HNil

  case class Piece(
                    facing: Vector3D,
                    position: Vector3D,
                    aim: Vector3D
                  )
  object Piece{
    implicit class Ops(val piece: Piece) extends AnyVal {
      def <+>(movement: Movement): Piece = {
        if (movement.axis.extract(piece.position) == movement.plane) {
          val matrix = movement.axis(movement.dir)
          piece.copy(facing = matrix * piece.facing,
            position = matrix * piece.position
          )
        } else piece
      }
    }
  }
  object Dir {
    implicit class Ops(val dir:Dir) extends AnyVal{
      def unary_! : Dir ={
        dir match {
          case `~>` => <~
          case `<~` => ~>
        }
      }
    }
    def random: Dir = {
      val random = new Random()
      random.nextInt(2) match {
        case 0 => ~>
        case 1 => <~
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




  implicit class MultV(val v1: Vector3D) extends AnyVal {
    def *(v2: Vector3D): Int = {
      v1.head * v2.head +
        v1.tail.head * v2.tail.head +
        v1.tail.tail.head * v2.tail.tail.head

    }
  }

  implicit class MultM(val value: Vector3D ::
    Vector3D ::
    Vector3D :: HNil) extends AnyVal {

    def *(vector: Vector3D) = {
      value.head * vector ::
        value.tail.head * vector ::
        value.tail.tail.head * vector :: HNil
    }
  }


  sealed trait Axis {
    def extract(position: Vector3D): Int

    def apply(dir: Dir): RMatrix
  }


  sealed trait Dir



  object ~> extends Dir

  object <~ extends Dir

  object X extends Axis {


    override def apply(dir: Dir): RMatrix = dir match {
      case `~>` =>
        (1 :: 0 :: 0 :: HNil) ::
          (0 :: 0 :: -1 :: HNil) ::
          (0 :: 1 :: 0 :: HNil) :: HNil
      case `<~` =>
        (1 :: 0 :: 0 :: HNil) ::
          (0 :: 0 :: 1 :: HNil) ::
          (0 :: -1 :: 0 :: HNil) :: HNil
    }

    override def extract(position: Vector3D): Int = position.head
  }

  object Y extends Axis {


    override def apply(dir: Dir): RMatrix = dir match {
      case `~>` =>
        (0 :: 0 :: 1 :: HNil) ::
          (0 :: 1 :: 0 :: HNil) ::
          (-1 :: 0 :: 0 :: HNil) :: HNil
      case `<~` =>
        (0 :: 0 :: -1 :: HNil) ::
          (0 :: 1 :: 0 :: HNil) ::
          (1 :: 0 :: 0 :: HNil) :: HNil
    }

    override def extract(position: Vector3D): Int = position.tail.head
  }

  object Z extends Axis {

    override def apply(dir: Dir): RMatrix = dir match {
      case `~>` =>
        (0 :: -1 :: 0 :: HNil) ::
          (1 :: 0 :: 0 :: HNil) ::
          (0 :: 0 :: 1 :: HNil) :: HNil
      case `<~` =>
        (0 :: 1 :: 0 :: HNil) ::
          (-1 :: 0 :: 0 :: HNil) ::
          (0 :: 0 :: 1 :: HNil) :: HNil
    }

    override def extract(position: Vector3D) = position.tail.tail.head
  }

  object Axis {
    def random = {
      val random = new Random()
      random.nextInt(3) match {
        case 0 => X
        case 1 => Y
        case 2 => Z
      }
    }
  }


}
