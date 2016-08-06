package icfp;

import spire.math.Rational

object Geometry {

  def angles(prev: Point, curr: Point, frontier: Seq[Point]): Seq[(Point, Double)] = {
    // need to compute dot product between curr-prev and a frontier-prev
    frontier map { f => 
      (f, (curr - prev).cosineAngleTo(f - prev))
    }
  }

}


object GeometryAnglesExample extends App {
  val prev = Point(0, 0)
  val current = Point(0, 1)
  val frontier = List(Point(-1, 2), Point(0, 2), Point(1, 2))
  println(Geometry.angles(prev, current, frontier))
}
