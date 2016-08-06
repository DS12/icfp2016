package icfp

/**
  * Created by htien on 8/5/16.
  */

import Geometry._

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
    def normalization: SilhouetteState = {
      val newProblem = Problem(silhouette, skeleton).normalize
      val newMap = map.map {
        case (i, list) => (i, list.map(_ - newProblem.silh.originPoint))
      }
      SilhouetteState(newProblem.silh, newProblem.skel, newMap)
    }

    val isSolved: Boolean = silhouette.polys.length == 1 && destination.forall(silhouette.polys.head.pts.contains(_))
    val isLegal: Boolean = true

    def vertices: Seq[Point] = edgeToVertex(skeleton.edges)

    val boundaries: Seq[LineSegment] = skeleton.boundary


    def unfold(edge: LineSegment): List[SilhouetteState] = ???


    def deFacet(facets: List[Facet], ske: Skeleton): (List[Facet], Skeleton) = {
      println(ske)
      if (ske.edges.length < 3) (facets, ske)
      else {
        val facetToGo = genFacet(ske.edges.head, ske.edges)
        val edgesToDel = facetToGo.edges.filter(ske.boundary.contains)
        val skeletonLeft = Skeleton(ske.edges.filter(!edgesToDel.contains(_)))
        deFacet(facetToGo :: facets, skeletonLeft)
      }
    }
  }

  //(silh, skel) => silhState
  def analyze(problem: Problem): SilhouetteState = {
    val initLabel = problem.skel.edges.flatMap(line => line.endpoints).distinct.zipWithIndex.map {
      case (p, i) => (i, List(p))
    }.toMap
    SilhouetteState(problem.silh, problem.skel, initLabel)
  }


  case class Solution(sil: SilhouetteState) {
    require(sil.isSolved)
    val facets: List[Facet] = sil.deFacet(List(), sil.skeleton)._1
    //    override def toString(): String = {
    //      ???
    //    }
  }

  def solve(problem: SilhouetteState): Solution = {

    if (problem.isSolved) Solution(problem)
    else {
      println("xxxxxxxxx")
      (for {
        edge <- problem.boundaries // boundary edges
        progress <- problem.unfold(edge).filter(_.isLegal)
      } yield solve(progress)).head
    }
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
1,1
0,1
4
0,0 1,0
1,0 1,1
1,1 0,1
0,0 0,1
    """

  val initToks = tokenize(ex)
  val problem = parseProblem.run(initToks).value._2
  val initialState: SilhouetteState = analyze(problem)
  val solution: Solution = icfp2016.solve(initialState)
  println(solution)

}
