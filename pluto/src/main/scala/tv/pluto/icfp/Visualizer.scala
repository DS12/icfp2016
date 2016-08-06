package tv.pluto.icfp

import java.io.ByteArrayInputStream

import tv.pluto.icfpGaming.Solution
import tv.pluto.icfp.Parser.parserProblem
import sys.process._
import ConvexHull.Tupler

import scala.io.Source

object Visualizer {

  def plotSolution(solution: Solution): Unit = {
    val points = solution.silhouette.zipWithIndex.map(_.swap).toMap
    val edges = solution.facets.flatMap {
      case ints => (ints.last :: ints).map(points).tuple
    }.map {
      case (p1, p2) => Edge(p1, p2)
    }

    visualize(edges)
  }

  def visualize(edges: Iterable[Edge]): Unit = {
    val input = edges.map(edge => {
      s"${edge.p1.x.toDouble} ${edge.p1.y.toDouble} ${edge.p2.x.toDouble} ${edge.p2.y.toDouble}"
    }).mkString("\n")

    val builder = "python ./plotting/plot_edges.py" #< new ByteArrayInputStream(input.getBytes())

    builder.!!
  }

  def main(args: Array[String]) {
//    visualize(List(Edge(Point(1, 2), Point(3, 4)), Edge(Point(2, -1), Point(3, 4)), Edge(Point(1, 2), Point(-3, 4))))

    val filename: String = "ls problems".lineStream.filter(_.contains("1452")).toList.head
    val problemCase: String = Source.fromFile("problems/" + filename).getLines().mkString("\n")

    val parsedEdges: Iterable[Edge] = parserProblem(problemCase).edges
    visualize(parsedEdges)
  }


}
