package DataStructures

import scala.io.Source

/**
  * Created by wzhao on 8/5/16.
  */


case class Input(hil: SilhouetteT, ske: SkeletonT) {
  override def toString = hil.toString + "\n" + ske.toString
}

case class ReadInput(fileName: String) {

  def readCoord(str: String) : FractionPointCoordinate = {
    if (str.contains("/")) {
      val Array(num, dem) = str.split("/")
      FractionPointCoordinate(num.trim.toInt, dem.trim.toInt)
    }
    else FractionPointCoordinate(str.toInt, 1)
  }

  def readVertex(lines: Array[String], lineNumber: Int) : PointT = {
    val Array(first, second) = lines(lineNumber).split(",")
    Point(readCoord(first), readCoord(second))
  }

  def readVertexList(lines: Array[String], n: Int, res: (List[PointT], Int)) : (List[PointT], Int) = {
    if (n == 0) res
    else {
      val newVertex = readVertex(lines, res._2)
      readVertexList(lines, n-1, (res._1 ::: List(newVertex), res._2+1) )
    }
  }

  def readPoly(lines: Array[String], start: Int) : (PolygonT, Int) = {
    val nVertex = lines(start).toInt
    val (polyList, cur) = readVertexList(lines: Array[String], nVertex, (List(), start+1) )
    (Polygon(polyList), cur)
  }

  def readPolyList(lines: Array[String], n: Int, res: (List[PolygonT], Int)) : (List[PolygonT], Int) = {
    if (n == 0) res
    else {
      val (newPoly, cur) = readPoly(lines, res._2)
      readPolyList(lines, n-1, (res._1 ::: List(newPoly), cur) )
    }
  }

  def readHil(lines: Array[String]) : (SilhouetteT, Int) = {
    val nPoly = lines(0).toInt
    val (listPoly, cur) = readPolyList(lines, nPoly, (List(), 1))
    (Silhouette(listPoly), cur)
  }

  def readVertex(str: String) : Point = {
    val Array(first, second) = str.split(",")
    Point(readCoord(first), readCoord(second))
  }

  def readSegment(lines: Array[String], start: Int) : Edge = {
    val Array(first, second) = lines(start).split(" ")
    val firstVertex = readVertex(first)
    val secondVertex = readVertex(second)
    Edge(firstVertex, secondVertex)
  }

  def readSegmentList(lines: Array[String], n: Int, res: (List[Edge], Int)) : (List[Edge], Int) = {
    if (n == 0) res
    else {
      val newSeg = readSegment(lines, res._2)
      readSegmentList(lines, n-1, (res._1 ::: List(newSeg), res._2+1))
    }
  }

  def readSke(lines: Array[String], start: Int) : Skeleton = {
    val nSeg = lines(start).toInt
    val (listSeg, cur) = readSegmentList(lines, nSeg, (List(), start+1))
    Skeleton(listSeg)
  }

  val lines = Source.fromFile(fileName).getLines.toArray
  val (hil: SilhouetteT, cur: Int) = readHil(lines)
  val ske : Skeleton = readSke(lines, cur)

  val problem = Problem(hil, ske)
}

object test extends App {
  val addr = "input.txt"
  val input = ReadInput(addr).problem
  println("Printing Sample Input ")
  println(input.toString)
}