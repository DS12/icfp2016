package icfp;

import scala.math.BigInt
case class Rational(n: BigInt, d: BigInt) {
  override def toString: String =
    if (n==0) "0"
    else if (n==d) "1"
    else n.toString + "/" + d.toString
}

case class Point(x: Rational, y: Rational)

object Point { def apply(coords: Rational*) = new Point(coords(0), coords(1)) }

case class Polygon(pts: Seq[Point]) {
  def isCCW: Boolean = ???
}

case class LineSegment(p1: Point, p2: Point) {
  def endpoints: Seq[Point] = p1 :: p2 :: Nil
}

case class Silhouette(polys: Seq[Polygon])
case class Skeleton(edges: Seq[LineSegment])


object Origami {
  type Problem = (Silhouette, Skeleton)

}
