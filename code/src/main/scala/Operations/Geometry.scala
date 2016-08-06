package Operations

import DataStructures.{Point, PointT, Polygon, LineFunction, FractionPointCoordinate, FractionT, EdgeT}

/**
  * Created by wzhao on 8/5/16.
  */
trait Geometry {

  def makeLine(p1: PointT, p2: PointT) : LineFunction = {
    val slope = (p1.y - p2.y) / (p1.x - p2.x)
    val intersect = p1.y - (slope * p1.x)
    LineFunction(slope, intersect)
  }

  def makeNormalLine(p1: PointT, p2: PointT) : LineFunction = {
    val func = makeLine(p1, p2)
    val center = Point((p1.x+p2.x)/2, (p1.y+p2.y)/2)
    val slope = FractionPointCoordinate(-1, 1) / func.slope
    val intersect = center.y - (center.x * slope)
    LineFunction(slope, intersect)
  }

//  def makeNormalLine(v: Vertex) : LineFunction = {
//    val func = makeLine(v.p, v.q)
//    val center = Point((v.q.x+v.q.x)/2, (v.q.y+v.q.y)/2)
//    val slope = FractionPointCoordinate(-1, 1) / func.slope
//    val intersect = center.y - (center.x * slope)
//    LineFunction(slope, intersect)
//  }

  def makeNormalLine(p: PointT, func: LineFunction) : LineFunction = {
    val slope = FractionPointCoordinate(-1, 1) / func.slope
    val intersect = p.y - (p.x * slope)
    LineFunction(slope, intersect)
  }

  def pointToLine(p: PointT, l: LineFunction) : FractionT = {
    val normalLine = makeNormalLine(p, l)
    (l.slope * p.x - p.y + l.intersect).abs /  (l.slope*l.slope + l.intersect*l.intersect).sqrt(8)
  }

  def pointToLine(p: PointT, l: LineFunction, normalLine: LineFunction) : FractionT = {
    (l.slope * p.x - p.y + l.intersect).abs /  (l.slope*l.slope + l.intersect*l.intersect).sqrt(8)
  }

  def intersectOfLine(l1: LineFunction, l2: LineFunction) : PointT = {
    val x = (l1.intersect - l2.intersect) / (l2.slope - l1.slope)
    val y = l1.slope * x + l1.intersect
    Point(x, y)
  }

  def distanceOfPoint(p1: PointT, p2: PointT) : FractionT = {
    val diffX = p1.x - p2.x
    val diffY = p1.y - p2.y
    (diffX*diffX + diffY*diffY).sqrt(8)
  }

  def findSymmetric(p: PointT, intersect: PointT) : PointT = {
    val x = intersect.x * 2 - p.x
    val y = intersect.y * 2 - p.y
    Point(x, y)
  }

  def flip(axis: (PointT, PointT), vertices: List[PointT]) : List[PointT] = {
    val line = makeLine(axis._1, axis._2)
    val normalLines = vertices.map(p => makeNormalLine(p, line))
    val intersectList = normalLines.map(l => intersectOfLine(line, l))
    vertices.zip(intersectList).map( { case (p1, p2) => findSymmetric(p1, p2) } )
  }

  def makeLineListOfPolygon(poly: Polygon) : List[(PointT, PointT)] = {
    val len = poly.vertices.length
    poly.vertices.zipWithIndex.combinations(2).filter({
      case (Seq((v1, d1), (v2, d2))) => (Math.abs(d1 - d2) == 1 || Math.abs(d1 - d2) == len - 1)
    }).map(sq => (sq(0)._1, sq(1)._1) ).toList
  }

  def isSeparable(l: LineFunction, vList1: List[PointT], vList2: List[PointT]) : Boolean = {
    val normalLines1 = vList1.map(p => makeNormalLine(p, l))
    val intersectList1 = normalLines1.map(l1 => intersectOfLine(l1, l))
    val y1List = intersectList1.map(p => p.y)
    val (max1, min1) = (y1List.max, y1List.min)

    val normalLines2 = vList2.map(p => makeNormalLine(p, l))
    val intersectList2 = normalLines2.map(l1 => intersectOfLine(l1, l))
    val y2List = intersectList2.map(p => p.y)
    val (max2, min2) = (y2List.max, y2List.min)

    if ( (max1 < min2) || (max2 < min1) ) true
    else false
  }

  def isIntersection(p1: Polygon, p2: Polygon) : Boolean = {
    val length1 = p1.vertices.length
    val lineList1 = makeLineListOfPolygon(p1)
    val normalLines1 = lineList1.map( {case (p1, p2) => makeNormalLine(p1, p2) } )

    val length2 = p2.vertices.length
    val vertexList2 = makeLineListOfPolygon(p2)
    val normalLines2 = vertexList2.map( {case (p1, p2) => makeNormalLine(p1, p2) } )

    (normalLines1 ::: normalLines2).exists(l => isSeparable(l, p1.vertices, p2.vertices))
  }


}
