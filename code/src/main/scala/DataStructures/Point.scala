package DataStructures

/**
  * Created by greddy on 8/5/16.
  */
trait PointT {

  val x:FractionT
  val y:FractionT

  override def toString:String =  x.toString + "," + y.toString

}

//case class Point(x:FractionPointCoordinate,y:FractionPointCoordinate) extends PointT
case class Point(x:FractionT,y:FractionT) extends PointT

object PointExample extends App {

  val f1 = FractionPointCoordinate(1,2)
  val f2 = FractionPointCoordinate(2,1)
  val point = Point(f1,f2)

  println(point)

}
