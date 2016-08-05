package icfp

import Origami._

object implicits {

  implicit def int2rat(i: Int): Rational = Rational(i, 1)

  implicit def intPair2Point(i: (Int, Int)): Point = Point(int2rat(i._1), int2rat(i._2))
}
