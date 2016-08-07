package icfp

/**
  * Created by htien on 8/5/16.
  */

import Geometry._
import vizTools.ProblemViewer.{GraphicProblemViewer, SkeletonViewer}

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
    val isLegal: Boolean =
      this.skeleton.boundaryPoints.flatMap {
        (p1: Point) => skeleton.boundaryPoints.map {
          (p2: Point) => p2.distanceFrom(p1)
        }
      }.forall(_ < math.sqrt(2.01)) // some room of error

    def vertices: Seq[Point] = edgeToVertex(skeleton.edges)

    val boundaries: Seq[LineSegment] = skeleton.boundary

    def unfold: List[SilhouetteState] = {
      this.skeleton.boundary.flatMap(unfold(_)).toList
    }

    def unfold(edge: LineSegment): Seq[SilhouetteState] = {
      // currently only deal with one facet
      val oneFacet = genFacet(edge, this.skeleton.edges)
      Seq(unfoldOneFacet(edge, oneFacet))
    }

    def unfoldOneFacet(edge: LineSegment, facet: Facet): SilhouetteState = {
      // add new lines
      val newEdges: Seq[LineSegment] = edge.reflect(facet).edges
      // erase old lines
      val movedEdges: Seq[LineSegment] = facet.edges.filter(!this.skeleton.boundary.contains(_))
      val newSkeleton: Skeleton = Skeleton((this.skeleton.edges.filter(!movedEdges.contains(_)) ++ newEdges).distinct)

      // if the points is in the Silhoette, then it must be a boundary point, so no points will be removed.
      val newPoints: Seq[Point] = newEdges.flatMap((edge: LineSegment) => edge.endpoints)
      val newSilhouette: Silhouette = Silhouette(
        this.silhouette.polys.map { (polygon: Polygon) =>
          if (polygon.pts.contains(edge.p1)) Polygon((polygon.pts ++ newPoints).distinct)
          else polygon
        }
      )

      def mapPointsTrans(oldPoints: List[Point]): List[Point] = oldPoints.map(edge.reflect).intersect(newPoints) ++ oldPoints
      val newMap: Map[Int, List[Point]] = this.map.map((tuple: (Int, List[Point])) => (tuple._1, mapPointsTrans(tuple._2)))

      SilhouetteState(newSilhouette, newSkeleton, newMap)
    }

    lazy val facets: Seq[Facet] = {
      val ske = this.skeleton
      val reversedEdges: Seq[LineSegment] = ske.edges.map((l: LineSegment) => LineSegment(l.p2, l.p1))
      val doubleSkeloton: Skeleton = Skeleton((ske.edges ++ reversedEdges).distinct)
      val allPossibleFacets: Seq[Facet] = ske.edges.map { (l: LineSegment) => genFacet(l, doubleSkeloton.edges) }
      allPossibleFacets.map(_.sort.sort).distinct
    }


    //    def deFacet(facets: List[Facet], ske: Skeleton): (List[Facet], Skeleton) = {
    //      facets.foreach(println)
    //      println("======")
    //      SkeletonViewer(ske)
    //      if (ske.edges.isEmpty || ske.edges.size < 3) (facets, ske)
    //      else {
    //        val facetToGo: Facet = genFacet(ske.edges.head, ske.edges)
    //        println(facetToGo)
    //        println("==")
    //        val edgesToDel: Seq[LineSegment] = facetToGo.edges.filter(ske.boundary.contains)
    //        println(edgesToDel)
    //        println("==")
    //        val skeletonLeft: Skeleton = Skeleton(ske.edges.filter(!edgesToDel.contains(_)))
    //        println(skeletonLeft)
    //        println("==")
    //        deFacet(facetToGo :: facets, skeletonLeft)
    //      }
    //    }

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
    sil.skeleton.edges.foreach(println)
//    val facets: List[Facet] = sil.deFacet(List(), sil.skeleton)._1

    val facets: Seq[Facet] = sil.facets
    facets.foreach(println)

    override def toString: String = {
      "is solved"
    }
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

  def solve(problems: Seq[SilhouetteState]): Solution = {
    val correct: Seq[Boolean] = problems.map(_.isSolved)
    if (correct.forall(_ == false)) solve(problems.flatMap(_.unfold))
    else {
      val firstCorrect: SilhouetteState = correct.zip(problems).filter(_._1 == true).head._2
      Solution(firstCorrect)
    }
  }

}

object exampleSolve extends App {

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

  val prob8 =
    """1
4
0,0
1/2,0
1/2,1/2
0,1/2
4
0,0 1/2,0
0,0 0,1/2
1/2,0 1/2,1/2
0,1/2 1/2,1/2
    """
  GraphicProblemViewer(prob8)

  val initToks = tokenize(prob8)
  val problem = parseProblem.run(initToks).value._2
  val initialState: SilhouetteState = analyze(problem)
  val solution: Solution = icfp2016.solve(Seq(initialState))
  //  println(solution)

}
