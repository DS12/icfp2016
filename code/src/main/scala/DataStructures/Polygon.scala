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

    def makeOneVertex(v: PointT) = v.toString + "\n"

    val tempString = vertices.length.toString + "\n" +
      vertices.map(v => makeOneVertex(v)).mkString("")
    val len = tempString.length
    tempString.slice(0, len - 1)

  }

}

case class Polygon(vertices: List[PointT]) extends PolygonT {}
