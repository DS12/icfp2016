package icfp

/**
  * Created by htien on 8/5/16.
  */

import Origami._

object icfp2016 {

  type Facet = Unit

  val fourVertices =
    """
       4
       0,0
       1,0
       1,1
       0,1
    """.stripMargin

  val x = 1

  val destination: Seq[Point] = OrigamiParse.parsePolygon.run(OrigamiParse.tokenize(fourVertices)).value._2.pts


  case class SilhouetteState(polys: Seq[Polygon], edges: Seq[LineSegment], map: Map[Point, Point]) {
    val isSolved: Boolean = polys.length == 1 && destination.forall(polys.head.pts.contains(_))
    val isLegal: Boolean = ???
    val vertices: Set[Point] = (polys.flatMap(_.pts) ++ edges.flatMap(_.endpoints)).toSet
    val facet: Seq[Facet] = ???
    val normalization: Silhouette = ???

    def unfold(edge: LineSegment): Silhouette = ???

  }

  case class Solution(vertices: Set[Point], facets: Seq[Facet], map: Map[Point, Point]) {
    //    override def toString(): String = {
    //      ???
    //    }
  }

  def parser(input: String): Silhouette = {
    ???
  }


  def solve(problem: SilhouetteState): Solution = {

    if (problem.isSolved) Solution(problem.vertices, problem.facet, problem.map)
    else ??? // DFS or BFS
  }


}

object solve extends App {

  import implicits._

  import icfp2016._


  val ex =
    """1
4
0,0
1,0
1/2,1/2
0,1/2
5
0,0 1,0
1,0 1/2,1/2
1/2,1/2 0,1/2
0,1/2 0,0
0,0 1/2,1/2
    """


  val problem = Silhouette(polygon, edges, Map[Point, Point]())
  val solution = icfp2016.solve(problem)
  println(solution.vertices)
  println(solution.facets)
  println(solution.vertices.map(solution.map.get(_)))

  //  val problem: Silhouette = parser(???)
  //  val solution: Solution = icfp2016.solve(problem)
  //  println(solution)
}
