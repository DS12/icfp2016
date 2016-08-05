package tv.pluto.icfp

import java.io.PrintWriter

import scala.io.Source

object Parser {

  def parseValue(segment: String): Double = {
    if (segment.contains("/")) {
      val parts = segment.split("/")
      parts(0).toDouble / parts(1).toDouble
    } else {
      segment.toDouble
    }
  }

  def parsePoint(segment: String): Point = {
    val parts = segment.split(",")

    Point(parseValue(parts(0)), parseValue(parts(1)))
  }

  def parseEdge(line: String): Edge = {
    val points = line.split(" ")

    Edge(parsePoint(points(0)), parsePoint(points(1)))
  }

  def parserProblem(string: String): Problem = {
    var lines = string.split('\n').toList.map(_.trim).filter(_.nonEmpty)
    val nPoly = lines.head.toInt

    lines = lines.tail
    // sorry for the for-loop
    val polygons = for {
      p <- (0 until nPoly.toInt).toList
    } yield {
      val nPoints = lines.head.toInt
      val points = lines.tail.take(nPoints)
      lines = lines.tail.drop(nPoints)

      points.map(parsePoint)
    }

    val edges = lines.tail.map(parseEdge).toSet

    Problem(polygons, edges)
  }

  def main(args: Array[String]) {
    val example =
      """
        |1
        |4
        |-1/2,-1/2
        |1/2,-1/2
        |1/2,1/2
        |-1/2,1/2
        |4
        |-1/2,-1/2 1/2,-1/2
        |-1/2,-1/2 -1/2,1/2
        |1/2,-1/2 1/2,1/2
        |-1/2,1/2 1/2,1/2
      """.stripMargin

    println(parserProblem(example))
  }
}

case class Problem(polygons: List[List[Point]], edges: Set[Edge])

case class Point(x: Double, y: Double)

case class Edge(p1: Point, p2: Point)



