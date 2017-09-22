package edu.caeus.rubik

import edu.caeus.rubik.defs._

import scala.collection.immutable

/**
  * Created by undertrail on 23/06/17.
  */
object Main {

  def main(args: Array[String]): Unit = {
    val scramble: immutable.Seq[Movement] =(0 until 20).map{ _ =>
      Movement.random
    }
    val unscramble=scramble.map(!_).reverse

    val scrambledCube=scramble.foldLeft(Predefined.R3x3x3){
      (cube,move)=>
        cube.map(_ <+> move)
    }
    val reassembledCube = unscramble.foldLeft(scrambledCube){
      (cube,move)=>
        cube.map(_ <+> move)
    }


    println(Predefined.R3x3x3 == reassembledCube)
  }
}
