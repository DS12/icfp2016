package tv.pluto.icfp

import spire.math.Rational

import scala.collection.mutable.ListBuffer

/**
  * Source: https://gist.github.com/tamland/8916429
  */
object ConvexHull {

  implicit class Tupler[T](seq: Seq[T]) {
    def tuple: Iterator[(T, T)] = seq.flatMap(x => List(x, x)).tail.grouped(2).filter(_.size == 2).map {
      case Seq(a, b) => (a, b)
    }
  }

  def edgesOnConvexHull(points: Set[Point]): Set[Edge] = {
    val hull: Seq[Point] = ConvexHull.convexHull(points.toSeq)

    (hull :+ hull.head).tuple.map {
      case (p1, p2) => Edge(p1, p2)
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
    val collinear = slope equals Rational.zero
    val leftTurn = slope < 0
    collinear || leftTurn
  }

}
