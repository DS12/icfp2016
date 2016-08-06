package DataStructures

/**
  * Created by greddy on 8/5/16.
  */

trait SolutionT {

  val srcPositions : Seq[Point]
  val facets : Seq[FacetT]
  val destPositions: Seq[Point]

}

case class Solution(srcPositions: Seq[Point], facets: Seq[FacetT], destPositions:Seq[Point]) extends SolutionT