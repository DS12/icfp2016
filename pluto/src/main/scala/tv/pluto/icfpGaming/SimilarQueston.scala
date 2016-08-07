package tv.pluto.icfpGaming

import tv.pluto.icfp.Parser._
import tv.pluto.icfp.Point.ordering
import tv.pluto.icfp._

import scala.io.Source
import scala.sys.process._

/**
  * Created by kamalgurala on 8/5/16.
  */
object SimilarQuestions {

  def main(args: Array[String]) {
    val problemFileNames: List[String] = "ls problems".lineStream.toList
    val allParsedProblems = problemFileNames.map {
      case fn =>
        (Source.fromFile("problems/" + fn).getLines().mkString("\n"), fn)
    }.filter(_._1.nonEmpty)
      .map {
        case (problem, fn) =>
          println(fn)
          (parserProblem(problem), fn)
      }

    val translatedParsedProblems = allParsedProblems
      .map(p => (translateProblem(p._1, computeMinPoint(p._1)), (p._2, computeMinPoint(p._1))))
      .map(x => (standardizeProblem(x._1), x._2))

    val recurringProblems = translatedParsedProblems
      .groupBy(_._1)
      .mapValues(x => x.map(y => y._2))
      .toList
      .filter(_._2.size > 1)
      .sortBy(_._2.size).reverse

    recurringProblems.foreach { x => println(x._2.size); println(x.toString); Visualizer.visualize(x._1.edges); }

    val recurringProblemsMap = recurringProblems.zipWithIndex.map(_.swap).toMap
  }

  def computeMinPoint(p: Problem): Point = {
    val polygon = p.polygons.head
    val minX = polygon.map(_.x).min
    val minY = polygon.map(_.y).min
    Point(minX, minY)
  }

  def translateProblem(p: Problem, min: Point): Problem = {
    Problem(translatePolygon(p.polygons, min), translateEdges(p.edges, min))
  }

  def translatePolygon(p: List[List[Point]], min: Point): List[List[Point]] = {
    p.map(ls => ls.map(p => p.subtract(min)))
  }

  def translateEdges(p: Set[Edge], min: Point): Set[Edge] = {
    p.map(e => Edge(e.p1.subtract(min), e.p2.subtract(min)))
  }

  def standardizeProblem(p: Problem): Problem = {
    val permutedPolygons = p.polygons.map {
      poly =>
        val minPoint = poly.min
        val minPointIndex = poly.indexOf(minPoint)
        val double = poly ++ poly
        double.slice(minPointIndex, minPointIndex + poly.size)
    }
    val swappedEdges = p.edges.map { e =>
      val ls = List(e.p1, e.p2).sorted
      Edge(ls.head, ls(1))
    }
    val sortedEdges = swappedEdges.toList.sortBy(e => (e.p1, e.p2)).toSet
    Problem(permutedPolygons, sortedEdges)

  }

}
