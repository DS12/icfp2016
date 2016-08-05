package tv.pluto.icfp

import scala.collection.mutable.ListBuffer

/**
  * Source: https://gist.github.com/tamland/8916429
  */
object ConvexHull {


  def edgesOnConvexHull(points: Set[Point]): Set[Edge] = {
    val hull: Seq[Point] = ConvexHull.convexHull(points.toSeq)

    (hull :+ hull.head).flatMap(x => List(x, x)).tail.grouped(2).filter(_.size == 2).map(_.toList).map {
      case Seq(p1, p2) => Edge(p1, p2)
    }.toSet
  }

  def convexHull(_points: Seq[Point]): Seq[Point] = {
    val points = _points.sortBy(_.x)
    val upper = halfHull(points)
    val lower = halfHull(points.reverse)
    upper.remove(0)
    lower.remove(0)
    upper ++: lower
  }

  def halfHull(points: Seq[Point]) = {
    val upper = new ListBuffer[Point]()
    for (p <- points) {
      while (upper.size >= 2 && leftTurn(p, upper(0), upper(1))) {
        upper.remove(0)
      }
      upper.prepend(p)
    }
    upper
  }

  def leftTurn(p1: Point, p2: Point, p3: Point) = {
    val slope = (p2.x - p1.x) * (p3.y - p1.y) - (p2.y - p1.y) * (p3.x - p1.x)
    val collinear = math.abs(slope) <= 1e-9
    val leftTurn = slope < 0
    collinear || leftTurn
  }

}
