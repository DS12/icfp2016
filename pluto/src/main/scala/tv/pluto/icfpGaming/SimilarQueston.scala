package tv.pluto.icfpGaming

import scala.io.Source
import tv.pluto.icfp._
import spire.math.Rational
import tv.pluto.icfp.Parser._

import sys.process._
import scala.io.Source

/**
  * Created by kamalgurala on 8/5/16.
  */
object SimilarQuestions {

  def main(args: Array[String]) {
    val problemFileNames: List[String] = "ls problems".lineStream.take(101).toList
    val allProblems = problemFileNames.map{
      fn =>
        val problem = Source.fromFile("problems/" + fn).getLines().mkString("\n")
        (problem,fn)
    }

    val allParsedProblems = allProblems.map(x => (parserProblem(x._1),x._2))

    val translatedParsedProblems = allParsedProblems.map(p => (translateProblem(p._1, computeMinPoint(p._1)),(p._2,computeMinPoint(p._1))))

    val recurringProblems = translatedParsedProblems
      .groupBy(_._1)
      .mapValues(x => x.map(y => y._2))
      .toList
      .filter(_._2.size > 1)
      .sortBy(_._2.size).reverse

    recurringProblems.foreach(println)

  }

  def computeMinPoint(p: Problem): Point = {
    val polygon = p.polygons.head
    val minX = polygon.map(_.x).min
    val minY = polygon.map(_.y).min
    Point(minX, minY)
  }

  def translateProblem(p: Problem, min: Point): Problem = {
    Problem(translatePolygon(p.polygons,min), translateEdges(p.edges,min))
  }

  def translatePolygon(p: List[List[Point]],min: Point): List[List[Point]] = {
    p.map(ls => ls.map(p => p.subtract(min)))
  }

  def translateEdges(p: Set[Edge],min: Point): Set[Edge] = {
    p.map(e => Edge(e.p1.subtract(min),e.p2.subtract(min)))
  }

}
