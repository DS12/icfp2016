package DataStructures

/**
  * Created by greddy on 8/5/16.
  */
trait PolygonT {

  val vertices: Seq[PointT]

  def numOfEdges:Int = vertices.size

  def isBounded:Boolean = false
  //def getArea:FractionT =

  override def toString : String = {
    vertices.length.toString + "\n" +
    vertices.map(_.toString).mkString("\n")
  }

}

case class Polygon(vertices: List[PointT]) extends PolygonT {}
