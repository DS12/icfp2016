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

  val p:PointT
  val q:PointT

  override def toString:String = p.toString + " " + q.toString

  def slope:FractionT = {
    //TODO Consider negative and positive infinites
    if ( q.x == p.x && q.y > q.x) FractionPointCoordinate(0,1)
    if ( q.x == p.x && q.y < q.x) FractionPointCoordinate(0,-1)

    else (q.y - p.y)/(q.x - p.x)
  }

  def intercept:FractionT = this.p.y - (this.p.x* this.slope)

  /**
    * Returns Tan(b-a)
    * b is the angle with x axis  and a is the angle of the given edge
    *
    * @param edge
    * @return
    */
  def slopeDiff(edge: EdgeT):FractionT = {

    val b = this.slope
    val a = edge.slope
    (b-a)/(a*b + 1)
  }

  /**
    * Area of a triangle formed by the edge with the given point)
    *
    * @param point
    * @return
    */
  def areaOfTriangle(point: PointT): FractionT = {
     areaOfTriangle(this.p,this.q,point)
  }

  def areaOfTriangleabs(point: PointT): FractionT = {
    areaOfTriangleabs(this.p,this.q,point)
  }

  /**
    * Area of triangle of three points
    *
    * @param a
    * @param b
    * @param c
    * @return
    */
  def areaOfTriangleabs(a: PointT,b:PointT,c:PointT):FractionT = {
    areaOfTriangle(a,b, c).abs
  }

  def areaOfTriangle(a: PointT,b:PointT,c:PointT):FractionT =
    (a.x * (b.y - c.y ) + b.x *(c.y - a.y) + c.x *(a.y - b.y)) / 2

  def pointLiesOnEdge(point: PointT):Boolean = areaOfTriangle(point).num == 0

  def neighbors(edges: Seq[EdgeT]):Seq[(EdgeT,FractionT)] =
    edges.filter( x => (x.p == this.p) || (x.q == this.q) || (x.p == this.q) || (x.q == this.q))
      .map{ x => (x,slopeDiff(x))}

  def pointsOnOppositeSide(point1: PointT, point2: PointT) =
    areaOfTriangle(point1)*areaOfTriangle(point2) < 0

  def pointsOnSameSide(point1: PointT,point2: PointT) = !pointsOnOppositeSide(point1,point2)

  def isParallel(edge: EdgeT):Boolean = this.slope == edge.slope

  def doesIntersect(edge: EdgeT):Boolean = isParallel(edge)

  //   assert(!e1.doesIntersect(e2),"Edges should not be parallel")
  def intersect(e1:EdgeT,e2:EdgeT):PointT = {

      val (p1,q1,p2,q2) = (e1.p,e1.q,e2.p,e2.q)
      val (x1,y1,x2,y2,x3,y3,x4,y4) = (p1.x,p1.y,q1.x,q1.y,p2.x,p2.y,q2.x,q2.y)

      val x = ((x1*y2) - (y1*x2))*(x3 - x4) - (x1-x2)*((x3*y4) - (y3*x4))
      val y = ((x1*y2) - (y1*x2))*(y3 - y4) - (y1-y2)*((x3*y4) - (y3*x4))
      val den = ((x1 -x2)*(y3-y4)) - ((y1-y2)*(x3-x4))
      val aa:FractionT= x / den
      val bb:FractionT= y / den
      Point(aa,bb)
  }

  def mirrorImageOfPoint(point: PointT):PointT = {
    val a = this.slope
    val b = this.intercept
    val (x1,y1)  = (point.x,point.y)
    val x2 = x1 +    ((y1 + x1/(a -b))/(a + a.inverse) - x1)*2
    val y2 = y1 +  a*(y1 + x1/(a -b))*2  /  ( a + a.inverse + b - y1)
    Point(x2,y2)
  }


}


case class Vertex(p:Point,q:Point) extends EdgeT

case class Edge(p:Point,q:Point) extends EdgeT


object EdgeExamples extends App {

  val f1 = FractionPointCoordinate(1,1)
  val f2 = FractionPointCoordinate(1,1)
  val point = Point(f1,f2) // (1,1)

  val f3 = FractionPointCoordinate(2,1)
  val f4 = FractionPointCoordinate(2,1)
  val point2 = Point(f3,f4) // (2,2)

  val f5 = FractionPointCoordinate(2,1)
  val f6 = FractionPointCoordinate(1,1)
  val point3 = Point(f5,f6) // (2,1)

  val f7 = FractionPointCoordinate(1,1)
  val f8 = FractionPointCoordinate(2,1)
  val point4 = Point(f7,f8) // (1,2)

  val f9 = FractionPointCoordinate(0,1)
  val f10 = FractionPointCoordinate(-1,1)
  val point5 = Point(f9,f9) // (1,2)
  val point6 = Point(f10,f10)

  val edge = Edge(point,point2)   // 45 degree
  val edge1 = Edge(point,point3) // x -axis
  val edge2 = Edge(point,point4) // y -axis
  val edge3 = Edge(point5, point6)
  // edge1 and egde2 are on opposite sides of edge0


  // Slopes
  //println("Slopes ", edge.slope, edge1.slope, edge2.slope)

  // SlopeDiff (tan(a-b))
  /*
  println( edge.slope)
  println(edge2.slope )
  println(edge.slope * edge2.slope + 1)

  println("Gcd of 0,0 is ", FractionPointCoordinate(0,0).reduce)
  println("Gcd of -1,1 is ", FractionPointCoordinate(-1,1).reduce)
  println("Gcd of 0,-1 is ", FractionPointCoordinate(0,-1).reduce)
  println("Gcd of 1,-2 is ", FractionPointCoordinate(1,-2).reduce)



  println(" Diff of Fractions is ", f7 -  FractionPointCoordinate(1,0) )
  println(" Diff of Fractions is ", FractionPointCoordinate(1,0) - f7 )

  println("Reduce of a and b is ", FractionPointCoordinate(-1,0).reduce)
  println("Reduce of a and b is ", FractionPointCoordinate(1,0).reduce)


  println(" Diff of Fractions is ", edge.slope - edge2.slope )
  println(" Diff of Fractions is ", edge2.slope - edge.slope )
  println(" Diff of Fractions is ", edge.slope - edge1.slope )
  println(" Diff of Fractions is ", edge1.slope - edge.slope )
  */
  //println("SlopeDiff", edge.slopeDiff(edge), edge.slopeDiff(edge2), edge1.slopeDiff(edge2), edge2.slopeDiff(edge1))

  //Area of the traingle

  println("Area 1 ", edge.areaOfTriangle(point3), edge.areaOfTriangle(point4) )

  println("Area abs 2", edge.areaOfTriangleabs(point3), edge.areaOfTriangleabs(point4) )

  println("Area of a triangle 1", edge.areaOfTriangleabs(point,point2,point3))
  println("Area of a triangle 2", edge.areaOfTriangleabs(point,point2,point4))

  println("Area of a triangle 3", edge.areaOfTriangleabs(point,point2,point2))

  println(FractionPointCoordinate(3,1) / 2)

  // Point Lies on Edge
  println("Point ", edge.pointLiesOnEdge(point2), edge.pointLiesOnEdge(point4))

  val n1 = edge.neighbors(List(edge2,edge1,edge3 ))
  println(n1)

  // Intersection point of lines
  val intersect = edge.intersect(edge2,edge1)
  println(intersect)

  println("Intercept of x_axis" , edge.intercept, edge1.intercept, edge2.intercept)

  println("Mirror Images of points ", edge.mirrorImageOfPoint(point3),edge.mirrorImageOfPoint(point4))

}

