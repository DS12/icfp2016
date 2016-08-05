<<<<<<< HEAD
package icfp

=======
>>>>>>> origin/waze-eng
/**
  * Created by htien on 8/5/16.
  */

import Origami._

object icfp2016 {

  val fourVertices =
    """
       4
       0,0
       1,0
       1,1
       0,1
    """.stripMargin

<<<<<<< HEAD
    val x = 1

=======
>>>>>>> origin/waze-eng
  val destination: Seq[Point] = OrigamiParse.parsePolygon.run(OrigamiParse.tokenize(fourVertices)).value._2.pts

  case class SilhouetteState(polys: Seq[Polygon], edges: List[LineSegment], map: Map[Point, Point]) {
    val isSolved: Boolean = polys.length == 1 && destination.forall(p => polys.head.contains(p))
    val isLegal: Boolean = ???
    val vertices: Set[Point] = (polys.flatMap(poly => poly) ++ edges.flatMap(_.endpoints)).toSet
    val facet: List[Facet] = ???
    val normalization: Silhouette = ???

    def unfold(edge: LineSegment): Silhouette = ???

  }

  case class Solution(vertices: List[Point], facets: List[Facet], map: Map[Point, Point]) {
    //    override def toString(): String = {
    //      ???
    //    }
  }

  def parser(input: String): Silhouette = {
    ???
  }


  def solve(problem: Silhouette): Solution = {
    if (problem.isSolved) Solution(problem.vertices, problem.facet, problem.map)
    else ??? // DFS or BFS
  }


}

object solve extends App {

  import icfp2016._

  // no parser, initiate by hand
  val polygon = List((0, 0), (1, 0), (1, 1), (0, 1))
  val edges = List(LineSeg((0, 0), (1, 0)),
    LineSeg((0, 0), (0, 1)),
    LineSeg((1, 0), (1, 1)),
    LineSeg((0, 1), (1, 1)))
  val problem = Silhouette(polygon, edges, Map[Point, Point]())
  val solution = icfp2016.solve(problem)
  println(solution.vertices)
  println(solution.facets)
  println(solution.vertices.map(solution.map.get(_)))

  //  val problem: Silhouette = parser(???)
  //  val solution: Solution = icfp2016.solve(problem)
  //  println(solution)
<<<<<<< HEAD
}
=======
}
>>>>>>> origin/waze-eng
