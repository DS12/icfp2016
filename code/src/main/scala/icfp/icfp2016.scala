package icfp

/**
  * Created by htien on 8/5/16.
  */


object icfp2016 {

  val fourVertices =
    """4
0,0
1,0
1,1
0,1
    """.stripMargin

  val destination: Set[Point] = OrigamiParse.parsePolygon.run(OrigamiParse.tokenize(fourVertices)).value._2.pts

  case class SilhouetteState(silhouette: Silhouette, skeleton: Skeleton, map: Map[Int, List[Point]]) {
    val isNormalized: Boolean = ???

    def normalization: SilhouetteState = {
      val newProblem = Problem(silhouette, skeleton).normalize
      val newMap = map.map {
        case (i, list) => (i, list.map(_ - newProblem.silh.originPoint))
      }
      SilhouetteState(newProblem.silh, newProblem.skel, newMap)
    }

    val isSolved: Boolean = silhouette.polys.size == 1 && destination.forall(silhouette.polys.head.pts.contains(_))
    val isLegal: Boolean = ???
    val vertices: Set[Point] = skeleton.edges.flatMap(_.endpoints).toSet
    val boundaries: Set[LineSegment] = skeleton.boundary


    def unfold(edge: LineSegment): List[SilhouetteState] = ???

    def unfoldOneFacet(edge: LineSegment, facet: Facet): SilhouetteState = {
      // add new lines
      val newEdges: Set[LineSegment] = edge.reflect(facet).edges
      // erase old lines
      val movedEdges: Set[LineSegment] = facet.edges.filter(!this.skeleton.boundary.contains(_))
      val newSkeleton: Skeleton = Skeleton(this.skeleton.edges.filter(!movedEdges.contains(_)) ++ newEdges)

      // if the points is in the Silhoette, then it must be a boundary point, so no points will be removed.
      val newPoints: Set[Point] = newEdges.flatMap((edge: LineSegment) => edge.endpoints)
      val newSilhouette: Silhouette = Silhouette(
          this.silhouette.polys.map { (polygon: Polygon) =>
            if (polygon.pts.contains(edge.p1)) Polygon(polygon.pts ++ newPoints)
            else polygon
          }
        )

      SilhouetteState(newSilhouette, newSkeleton, ???)
    }


    def deFacet(facets: List[Facet], ske: Skeleton): (List[Facet], Skeleton) = {
      def findFacet(edge: LineSegment): Facet = {
        def choose(available: Set[LineSegment]): LineSegment = ???
        val available = ske.edges.filter(ls => ls.endpoints.contains(edge.p2) && !ls.endpoints.contains(edge.p1))
        val theRightWay = choose(available)
        ??? : Facet
      }

      if (ske.edges.size < 3) (facets, ske)
      else {
        val facetToGo = findFacet(ske.edges.head)
        val edgesToDel = facetToGo.edges.intersect(boundaries)
        val skeletonLeft = Skeleton(ske.edges.filter(!edgesToDel.contains(_)))
        deFacet(findFacet(ske.edges.head) :: facets, skeletonLeft)
      }
    }

  }

  //(silh, skel) => silhState
  def analyze(prob: Problem): SilhouetteState = {
    val initLabel = prob.skel.edges.flatMap(line => line.endpoints).zipWithIndex.map {
      case (p, i) => (i, List(p))
    }.toMap
    SilhouetteState(prob.silh, prob.skel, initLabel)
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

  import OrigamiParse._
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

  val initToks = tokenize(ex)
  println(s"initToks = $initToks")
  val prob = parseProblem.run(initToks).value._2
  println(prob)

  val problem: SilhouetteState = analyze(prob)
  val solution: Solution = icfp2016.solve(problem)
  println(solution)

}
