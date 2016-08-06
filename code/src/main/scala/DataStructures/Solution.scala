package DataStructures

/**
  * Created by greddy on 8/5/16.
  */

trait SolutionT {

  val srcPositions : Seq[PointT]
  val facets : Seq[FacetT]
  val destPositions: Seq[PointT]

  override def toString:String = {
    srcPositions.length + "\n" +
    srcPositions.map(_.toString).mkString("\n") + "\n" +
    facets.length + "\n" +
    facets.map(_.toString).mkString("\n") + "\n" +
    destPositions.map(_.toString).mkString("\n")
  }

}

case class Solution(srcPositions: Seq[PointT], facets: Seq[FacetT], destPositions:Seq[PointT]) extends SolutionT


object ProblemSolutionPrintTest extends App {

  val addr = "input.txt"
  val input = ReadInput(addr).problem

  val srcPositions = input.silhouette.polyList.head.vertices
  val dstPositions = srcPositions

  println("Printing Sample Input ")
  println("*********************")
  println("*********************")
  println(input.toString)
  println("*********************")
  println("*********************")

  val facet1 = Facet(List(0,1,5,4))
  val facet2 =  Facet(List(2,6,5))
  val facet3 = Facet(List(4,5,3))
  val facet4 = Facet(List(4,5,6,3))

  println(facet1)
  println(facet2)
  println(facet4)

  val facets = List(facet1,facet2,facet3)
  val solution = Solution(srcPositions,facets,dstPositions)
  println("*********************")
  println("*********************")
  println(solution)
  println("*********************")
  println("*********************")

}