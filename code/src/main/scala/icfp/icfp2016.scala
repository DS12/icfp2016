package icfp

/**
  * Created by htien on 8/5/16.
  */

import Origami._

object icfp2016 {

  val fourVertices =
    """4
0,0
1,0
1,1
0,1
    """.stripMargin

  val destination: Seq[Point] = OrigamiParse.parsePolygon.run(OrigamiParse.tokenize(fourVertices)).value._2.pts

  case class SilhouetteState(silhouette: Silhouette, skeleton: Skeleton, map: Map[Int, List[Point]]) {
    val isNormalized: Boolean = ???
    val normalization: Silhouette = ???

    val isSolved: Boolean = silhouette.polys.length == 1 && destination.forall(silhouette.polys.head.pts.contains(_))
    val isLegal: Boolean = ???
    val vertices: Set[Point] = (skeleton.edges.flatMap(_.endpoints)).toSet
    val boundaries: Set[LineSegment] = skeleton.boundary


    def unfold(edge: LineSegment): List[SilhouetteState] = ???

    def deFacet(ske: Skeleton): (Facet, Skeleton) = {
      ???
    }

  }

  //(silh, skel) => silhState
  def analyze(prob: Problem): SilhouetteState = {
    val initLabel = prob._2.edges.flatMap(line => line.endpoints).distinct.zipWithIndex.map {
      case (p, i) => (i, List(p))
    }.toMap
    SilhouetteState(prob._1, prob._2, initLabel)
  }


  case class Solution(sil: SilhouetteState) {
    require(sil.isSolved)
    val facets: Seq[Facet] = ???
    override def toString(): String = {
      ???
    }
  }

  def solve(problem: SilhouetteState): Solution = {

    if (problem.isSolved) Solution(problem)
    else {
      for {
        edge <- problem.boundaries // boundary edges
        progress <- problem.unfold(edge).filter(_.isLegal)
      } yield solve(progress)
    }.head
  }


}

object solve extends App {

  import implicits._

  import icfp2016._
  import OrigamiParse._


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

  val initToks = tokenize(ex)
  println(s"initToks = $initToks")
  val prob = parseProblem.run(initToks).value._2
  println(prob)

  val problem: SilhouetteState = analyze(prob)
  val solution: Solution = icfp2016.solve(problem)
  println(solution)

}
