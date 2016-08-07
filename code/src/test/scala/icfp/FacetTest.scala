package icfp

import icfp.Geometry.{Point, ccwPoints}
import org.scalatest.FunSuite

/**
  * Created by tianxia on 8/6/16.
  */
class FacetTest extends FunSuite {
  test("sort method of the Facet") {

    val testPoints1: Seq[Point] = Seq(Point(1, 0), Point(0, 0), Point(1, 1), Point(0, 1))
    val ccw1: Seq[Point] = ccwPoints(testPoints1).sortedPoints

    val testPoints2: Seq[Point] =
      Seq(Point(1.0 / 2, 1.0 / 2), Point(1.0 / 2, 0), Point(0, 1.0 / 2), Point(1.0 / 2, 1), Point(0, 1), Point(0, 0))
    val ccw2: Seq[Point] = ccwPoints(testPoints2).sortedPoints

    assert(Facet(ccwPoints(testPoints1)).sort == Facet(ccwPoints(ccw1)))

    assert(Facet(ccwPoints(testPoints2)).sort == Facet(ccwPoints(ccw2)))
  }
}
