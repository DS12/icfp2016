import util.Random._

/**
  * Created by hneeb on 8/5/16.
  *
  * Purpose of this object is to create an arbitrarily small square in the 
  * ICFP 2016 functional programming challenge
  */
object ProblemGenerator {

  /* problem parameters */
  val numSplits = 8

  /* helper function to find the gcd of two numbers */
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  /* helper function to add fractions */
  def addFrac(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    val aNum = a._1
    val aDen = a._2
    val bNum = b._1
    val bDen = b._2
    val newDen = aDen * bDen
    val newNum = (aNum * bDen) + (bNum * aDen)
    val GCD = gcd(newDen, newNum)
    (newNum / GCD, newDen / GCD)
  }

  /* Helper function to parse points into a solution text */
  def processPoints(point: ((Int, Int), (Int, Int))): String = {
    val xNum = point._1._1.toString
    val xDen = point._1._2
    val yNum = point._2._1.toString
    val yDen = point._2._2
    val xAddRight = if (xDen == 1) "" else "/" ++ xDen.toString
    val yAddRight = if (yDen == 1) "" else "/" ++ yDen.toString
    xNum ++ xAddRight ++ "," ++ yNum ++ yAddRight
  }
 
  /* Helper function to help parse facets into solution text */
  def parseFacets(facetTup: (Int, Int, Int, Int)): String = {
    val x1 = facetTup._1.toString ++ " "
    val x2 = facetTup._2.toString ++ " "
    val x3 = facetTup._3.toString ++ " "
    val x4 = facetTup._4.toString
    "4 " ++ x1 ++ x2 ++ x3 ++ x4
  }

  /* helper function to print solution in correct format */
  def printProblem(numPoints: String, probPoints: Array[String], numFacets: String, facets: Array[String], solPoints: Array[String]) = {
    println(numPoints)
    probPoints.map(x => println(x))
    println(numFacets)
    facets.map(x => println(x))
    solPoints.map(x => println(x))
  }

  /* split the x and y axis evenly according to the number of splits*/
  val xSplits = Array.range(0, numSplits + 1, 1).
    map { x =>
      val GCD = gcd(x, numSplits)
      (x / GCD, numSplits / GCD)
    }
  val ySplits = xSplits

  /* cartesian product of x and y splits */
  val allPoints = ySplits.flatMap(y => xSplits.map(x => (x, y)))

  /* arries of varies final sillohette squares */
  val endPoint = addFrac((1, 2), (1, numSplits))
  val bottom = Array(((1, 2), (1, 2)), (endPoint, (1, 2)))
  val top = Array(((1, 2), endPoint), (endPoint, endPoint))

  val numPoints = allPoints.size

  /* Slice the arrays so they appear in same y - value chunks */
  val slices = Array.range(0, numPoints, numSplits + 1).
    map(idx => allPoints.slice(idx, idx + numSplits + 1))

  val numSlices = slices.size

  /* Generate the solutions to the problems */
  val solSliceBottom = Array.range(0, numSplits + 1, 1).map(idx => bottom((idx % 2)))
  val solSliceTop = Array.range(0, numSplits + 1, 1).map(idx => top((idx % 2)))
  val solArr = Array(solSliceBottom, solSliceTop)
  val solution = Array.range(0, numSplits + 1, 1).flatMap(idx => solArr((idx % 2)))

  /* Create the facets for the solution */
  val facets = Array.range(1, numPoints - numSlices, 1).
    filter(x => x % numSlices != 0).
    map(x => x - 1).
    map(x => (x, x + 1, x + numSlices + 1, x + numSlices))

  /* Create text form of solution to bew saved to file */
  val probPrint = allPoints.map(point => processPoints(point))
  val solPrint = solution.map(point => processPoints(point))
  val facetPrint = facets.map(facet => parseFacets(facet))
  val numPointsPrint = numPoints.toString
  val numFacets = facetPrint.size.toString

  printProblem(numPointsPrint, probPrint, numFacets, facetPrint, solPrint)

}