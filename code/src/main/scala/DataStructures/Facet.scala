package DataStructures

/**
  * Created by greddy on 8/5/16.
  */

trait FacetT {

  val edges: Seq[Int]

  override def toString:String = {
    edges.size + " " +  edges.map(_.toString).mkString(" ")
  }

}

case class Facet(edges:Seq[Int])  extends FacetT


object FacetExample extends App {

  val facet1 = Facet(List(0,1,5,4))
  val facet2 =  Facet(List(2,6,5))
  val facet3 = Facet(List(4,5,3))
  val facet4 = Facet(List(4,5,6,3))

  println(facet1)
  println(facet2)
  println(facet4)

  val facets = Seq(facet1,facet2,facet3)


}