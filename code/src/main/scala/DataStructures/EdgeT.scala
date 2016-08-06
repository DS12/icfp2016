package DataStructures

/**
  * Created by greddy on 8/5/16.
  */

/**
  * An unique edge/ line could be constructed from two points
  * p and q are the two points which define the edge
  * p and q need to be boundaries of the edge
  * p and q are inclusive
  */

trait EdgeT {

  val p:Point
  val q:Point

  override def toString:String = p.toString + " " + q.toString

}

case class Vertex(p:Point,q:Point) extends EdgeT

case class Edge(p:Point,q:Point) extends EdgeT

