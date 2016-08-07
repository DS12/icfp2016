package tv.pluto.icfpQuestions

import spire.math.Rational
import tv.pluto.icfp.Point

/**
  * Parses a ready-made question and translates it
  */
object ParseMove {

  def main(args: Array[String]): Unit = {

    val orig = """
      |0,0
      |0,11/20
      |-9/20,11/20
      |1/10,0
      |1/10,9/20
      |-9/20,9/20
      |0,0
      |0,11/20
      |-9/20,11/20
      |1/10,0
      |1/10,9/20
      |-9/20,9/20
      |0,0
      |0,11/20
      |-9/20,11/20
      |1/10,0
      |1/10,9/20
      |-9/20,9/20
      |0,0
      |0,11/20
      |-9/20,11/20
      |1/10,0
      |1/10,9/20
      |-9/20,9/20
      |0,0
      |0,11/20
      |-9/20,11/20
      |1/10,0
      |1/10,9/20
      |-9/20,9/20
      |0,0
      |0,11/20
      |-9/20,11/20
    """.stripMargin.trim.split("\n").toList

    val origRational = orig.map{ line => line.split(",") }
      .map{ lineArray => lineArray.map{ num => Rational(num) } }
      .map{ twoRats => Point(twoRats(0), twoRats(1)) }

    val moved = origRational.map{ aPoint: Point => aPoint.add(Point(2,3)) }

    moved foreach println

  }

}
