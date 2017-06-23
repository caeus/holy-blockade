package edu.caeus.rubik

import shapeless._

/**
  * Created by undertrail on 23/06/17.
  */

object defs {
  type Vector3D = Int :: Int :: Int :: HNil

  private implicit class Mult(v1: Vector3D) {
    def *(v2: Vector3D): Int = {
      v1.head * v2.head +
        v1.tail.head * v2.tail.head +
        v1.tail.tail.head * v2.tail.tail.head

    }
  }

  sealed trait RMatrix {


    def *(vector: Vector3D): Vector3D = {
      value.head * vector ::
        value.tail.head * vector ::
        value.tail.tail.head * vector :: HNil
    }

    def value: (Vector3D) ::
      (Vector3D) ::
      (Vector3D) :: HNil
  }

  sealed trait Axis {
    def extract(v: Vector3D): Int

    def + : RMatrix

    def - : RMatrix
  }

  sealed trait Dir {
    def matrix(a: Axis): RMatrix
  }

  object Dir {

    object + extends Dir {
      override def matrix(a: Axis): RMatrix = a.+
    }

    object - extends Dir {
      override def matrix(a: Axis): RMatrix = a.-
    }

  }

  object RMatrix {


    object X extends Axis {

      object + extends RMatrix {
        def value = (1 :: 0 :: 0 :: HNil) ::
          (0 :: 0 :: -1 :: HNil) ::
          (0 :: 1 :: 0 :: HNil) :: HNil
      }

      object - extends RMatrix {
        def value = (1 :: 0 :: 0 :: HNil) ::
          (0 :: 0 :: 1 :: HNil) ::
          (0 :: -1 :: 0 :: HNil) :: HNil
      }

      override def extract(v: Vector3D): Int = {
        v.head
      }
    }

    object Y extends Axis {

      object + extends RMatrix {
        def value = (0 :: 0 :: 1 :: HNil) ::
          (0 :: 1 :: 0 :: HNil) ::
          (-1 :: 0 :: 0 :: HNil) :: HNil
      }

      object - extends RMatrix {
        def value = (0 :: 0 :: -1 :: HNil) ::
          (0 :: 1 :: 0 :: HNil) ::
          (1 :: 0 :: 0 :: HNil) :: HNil
      }

      override def extract(v: ::[Int, ::[Int, ::[Int, HNil]]]): Int = v.tail.head
    }

    object Z extends Axis {

      object + extends RMatrix {
        def value = (0 :: -1 :: 0 :: HNil) ::
          (1 :: 0 :: 0 :: HNil) ::
          (0 :: 0 :: 1 :: HNil) :: HNil
      }

      object - extends RMatrix {
        def value = (0 :: 1 :: 0 :: HNil) ::
          (-1 :: 0 :: 0 :: HNil) ::
          (0 :: 0 :: 1 :: HNil) :: HNil
      }

      override def extract(v: ::[Int, ::[Int, ::[Int, HNil]]]): Int = v.tail.tail.head
    }


  }

}
