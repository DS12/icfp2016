package Operations

import DataStructures.{Point, PointT, Polygon, LineFunction, FractionPointCoordinate, FractionT, EdgeT, Edge}

/**
  * Created by wzhao on 8/5/16.
  */
trait Geometry {

  def makeLine(p1: PointT, p2: PointT) : LineFunction = {
    val slope = (p1.y - p2.y) / (p1.x - p2.x)
    val intersect = p1.y - (slope * p1.x)
    LineFunction(slope, intersect)
  }

  def makeLine(edge: EdgeT) : LineFunction = {
    val slope = (edge.p.y - edge.q.y) / (edge.p.x - edge.q.x)
    val intersect = edge.p.y - (slope * edge.p.x)
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

  def makeLineMapOfPolygon(poly: Polygon) : Map[LineFunction, Edge] = ???

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

  def isIntersect(p1: Polygon, p2: Polygon) : Boolean = {
    val length1 = p1.vertices.length
    val lineList1 = makeLineListOfPolygon(p1)
    val normalLines1 = lineList1.map( {case (p1, p2) => makeNormalLine(p1, p2) } )

    val length2 = p2.vertices.length
    val vertexList2 = makeLineListOfPolygon(p2)
    val normalLines2 = vertexList2.map( {case (p1, p2) => makeNormalLine(p1, p2) } )

    (normalLines1 ::: normalLines2).exists(l => isSeparable(l, p1.vertices, p2.vertices))
  }

  def isParallel(edge1: EdgeT, edge2: EdgeT) : Boolean = {
    (edge1.p.x - edge1.q.x) / (edge1.p.y - edge1.q.y) == (edge2.p.x - edge2.q.x) / (edge1.p.y - edge2.q.y)
  }

  def isParallel(line1: LineFunction, line2: LineFunction) : Boolean = line1.slope == line2.slope

  def isIntersect(edge1: EdgeT, edge2: EdgeT) : (Boolean, PointT) = {
    if (isParallel(edge1, edge2)) (false, edge1.p)
    else {
      val line1 = makeLine(edge1)
      val line2 = makeLine(edge2)
      val intersect = intersectOfLine(line1, line2)
      if (isOnLine(intersect, line1) && isOnLine(intersect, line2)) (true, intersect)
      else (false, edge1.p)
    }
  }

  def isIntersect(line1: LineFunction, line2: LineFunction) : (Boolean, PointT) = {
    val tmpPoint = Point(FractionPointCoordinate(1,1), FractionPointCoordinate(1,1))

    if (isParallel(line1, line2)) (false, tmpPoint)
    else {
      val intersect = intersectOfLine(line1, line2)
      if (isOnLine(intersect, line1) && isOnLine(intersect, line2)) (true, intersect)
      else (false, tmpPoint)
    }
  }

  def isIntersect(l: LineFunction, poly: Polygon) : (Boolean, PointT) = {
    val tmpPoint = Point(FractionPointCoordinate(1,1), FractionPointCoordinate(1,1))
    val lineList = makeLineListOfPolygon(poly)
    val lineListFiltered = lineList.filter({ case (p1, p2) => isIntersect(l, makeLine(p1, p2))._1})
    if (lineListFiltered.length == 0) (false, tmpPoint)
    else isIntersect(l, makeLine(lineListFiltered.head._1, lineListFiltered.head._2))
  }

  def isInPolygon(p: PointT, poly: Polygon) : Boolean = {
    val minX = poly.vertices.map(v => v.x).min
    val minY = poly.vertices.map(v => v.y).min
    val maxX = poly.vertices.map(v => v.x).max
    val maxY = poly.vertices.map(v => v.y).max
    if (p.x <= minX || p.x >= maxX || p.y <= minY || p.y >= maxY) false
    else true
  }

  def isOnLine(p: PointT, l: LineFunction) : Boolean = l.slope * p.x + l.intersect == p.y

  def onWhichLine(p: PointT, poly: Polygon) : EdgeT = {
    val lineList = makeLineListOfPolygon(poly)
    val pointPair = lineList.filter(edge => isOnLine(p, makeLine(edge._1, edge._2))).head
    Edge(pointPair._1, pointPair._2)
  }

  def onWhichLine(p: PointT, lineList: List[(PointT, PointT)]) : EdgeT = {
    //val lineList = makeLineListOfPolygon(poly)
    val pointPair = lineList.filter(edge => isOnLine(p, makeLine(edge._1, edge._2))).head
    Edge(pointPair._1, pointPair._2)
  }

  def findInnerPointIndex(points: Array[PointT], poly: Polygon) : Int = {
    def iter(ind: Int) : Int = {
      if (isInPolygon(points(ind), poly)) ind
      else iter(ind+1)
    }
    iter(0)
  }

  def findIndexOfPointOnPolygon(point: PointT, poly: Polygon) : Int = {
    poly.vertices.indexWhere(p => p == point)
  }

  def intersection(p1: Polygon, p2: Polygon) : Polygon = {
    val vertexList1 = p1.vertices.toArray
    val vertexList2 = p2.vertices.toArray

    val lineList1 = makeLineListOfPolygon(p1)
    val lineList2 = makeLineListOfPolygon(p2)

    val firstInd = findInnerPointIndex(vertexList1, p2)
    val firstPoint = vertexList1(firstInd)

    def iter(currentPolygon: Polygon, currentInd: Int, backupPolygon: Polygon,
             currentLineList: List[(PointT, PointT)], backupLineList: List[(PointT, PointT)],
             res: List[PointT]) : List[PointT] = {
      val currentList = currentPolygon.vertices.toArray
      val backupList = backupPolygon.vertices.toArray
      if (currentList(currentInd) == firstPoint) res
      else {
        val nextInd = currentInd + 1
        if (nextInd == currentList.length) throw new Error("Index out of bound of polygon vertices")
        val (check, intersect) = isIntersect(makeLine(currentList(currentInd), currentList(nextInd)), currentPolygon)
        if (check) {
          val potentialPoints = currentLineList.filter( { case (point1, point2)  =>
            isOnLine(intersect, makeLine(point1, point2) ) } ).head
          val nextPointInBackUp = {
            if (isInPolygon(potentialPoints._1, backupPolygon)) potentialPoints._1
            else potentialPoints._2
          }
          val nextIndInBackup = findIndexOfPointOnPolygon(nextPointInBackUp, backupPolygon)
          iter(backupPolygon, nextIndInBackup, currentPolygon, backupLineList, currentLineList,
                res ::: List(intersect, nextPointInBackUp))
        }

        else {
          iter(currentPolygon, nextInd, backupPolygon, currentLineList, backupLineList, res ::: List(currentList(nextInd)))
        }
      }
    }

    val result = iter(p2, firstInd, p1, lineList2, lineList1, List(firstPoint))
    Polygon(result)
  }
}
