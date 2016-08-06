package tv.pluto.icfpGaming

import spire.math.Rational
import tv.pluto.icfp.{Edge, Point, Visualizer}
import tv.pluto.icfp.ConvexHull.Tupler

import scala.collection.generic.CanBuildFrom
import scala.util.Random

object Problems {

  def generateStripe(stripes: Int): Solution = {
    val lowerTriangular = for {
      i <- 1 to stripes
    } yield {
      val xs = Rational.zero
      val ys = Rational(BigInt(i), BigInt(stripes))

      val xe = ys
      val ye = xs

      Edge(Point(xs, ys), Point(xe, ye))
    }

    val upperTriangular = for {
      i <- 1 until stripes
    } yield {
      val xs = Rational(BigInt(i), BigInt(stripes))
      val ys = Rational.one

      val xe = ys
      val ye = xs

      Edge(Point(xs, ys), Point(xe, ye))
    }

    val lines = lowerTriangular ++ upperTriangular

    val linesNumber = lines.zipWithIndex.flatMap {
      case (e: Edge, i: Int) => List(e.p1, e.p2).map(p => (p, i))
    }.toMap

    val centers: Map[Int, Point] = lines.map(_.center).zipWithIndex.map(_.swap).toMap

    val skeleton = List(Point(0, 0)) ++ lines.flatMap(e => List(e.p1, e.p2)) ++ List(Point(1, 1))
    val indices = skeleton.zipWithIndex.toMap

    Visualizer.visualize(lines ++ skeleton.map(p => Edge(p, p add Point(Rational(0.1), Rational(0.1)))))

    val facets = {
      val first = Point(0, 0)
      val Edge(second, third) = lines.head

      List(first, second, third).map(x => indices(x))
    } :: {
      val Edge(second, third) = lines.last
      val last = Point(1, 1)

      List(second, third, last).map(x => indices(x))
    } :: lines.tuple.map {
      case (l: Edge, r: Edge) =>
        List(l.p1, l.p2, r.p2, r.p1).map(x => indices(x))
    }.toList

    val off = Rational(1L, stripes)

    val silhouette = (List(Point(0, 0)) ++ lines ++ List(Point(1, 1))).grouped(2).flatMap {
      case List(point: Point, edge@Edge(p1, p2)) if point.x == Rational.zero && point.y == Rational.zero =>
        List(
          (Point(0, 0), indices(point)),
          (p1, indices(p1)),
          (p2, indices(p2))
        )
      case List(e1@Edge(p1x, p1y), e2@Edge(p2x, p2y)) =>
        val c = e1.center
        List(
          (p1x subtract c, indices(p1x)),
          (p1y subtract c, indices(p1y)),
          (p2x subtract c, indices(p2x)),
          (p2y subtract c, indices(p2y))
        )
      case List(point: Point) =>
        List((Point(0, 0), indices(point)))
    }.toList.sortBy(_._2).map(_._1)

    Visualizer.visualize(skeleton.zip(silhouette).map {
      case (Point(x1, y1), Point(x2, y2)) =>
        val d: Rational = 0.001 * new Random().nextInt(100)
        Edge(Point(x1 + d, y1), Point(x2 + d, y2))
    }.toList)


    val pSk = indices.map(_.swap).toMap
    val pSi = silhouette.zipWithIndex.map(_.swap).toMap

    val xx = facets.map {
      case ints => (ints.last :: ints).map(pSi).tuple.toList.map {
        case (p1, p2) => Edge(p1, p2)
      }
    }

    Visualizer.visualize(xx.flatten)

    Solution(skeleton, facets, silhouette)
  }

  def main(args: Array[String]) {
    println(generateStripe(8))
  }

}


