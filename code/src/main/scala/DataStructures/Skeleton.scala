package DataStructures

/**
  * Created by greddy on 8/5/16.
  */

trait SkeletonT {

  val edges:Seq[EdgeT]
  val numOfEdges = edges.size

  override def toString:String = {
    edges.length + "\n" + edges.map(_.toString).mkString("\n")
  }

}

case class Skeleton(edges: List[EdgeT]) extends SkeletonT {}




