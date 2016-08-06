package DataStructures

/**
  * Created by greddy on 8/5/16.
  */

trait FacetT {
  val edges: Seq[Point]
}

case class Facet(n:Int,edges:Seq[Int])