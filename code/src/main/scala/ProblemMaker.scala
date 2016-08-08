import icfp.Geometry.{LineSegment, Point}

/**
  * Created by tianxia on 8/6/16.
  */
object ProblemMaker3 extends App {
  val source = Array(
    Point(0, 0),
    Point(1.0 / 2, 0),
    Point(1, 0),
    Point(1, 3.0 / 8),
    Point(1, 1.0 / 2),
    Point(1, 5.0 / 8),
    Point(1, 1),
    Point(1.0 / 2, 1),
    Point(1.0 / 4, 1),
    Point(0, 1),
    Point(0, 3.0 / 4),
    Point(0, 1.0 / 2),
    Point(0, 1.0 / 4),
    Point(1.0 / 4, 1.0 / 2),
    Point(1.0 / 2, 1.0 / 2),
    Point(3.0 / 4, 1.0 / 2)
  )

  val fold1 = source.map { (p: Point) =>
    if (p.y > 1.0 / 2) LineSegment(source(4), source(11)).reflect(p)
    else p
  }

  val fold2 = fold1.map { (p: Point) =>
    if (p.x < 1.0 / 2) LineSegment(source(1), source(14)).reflect(p)
    else p
  }

  val destination = (0 to 15).map { (i: Int) =>
    if (i == 10 || i == 9) LineSegment(fold2(8), fold2(13)).reflect(fold2(i))
    else if (i == 11) LineSegment(fold2(12), fold2(13)).reflect(fold2(i))
    else if (i == 5) LineSegment(fold2(6), fold2(15)).reflect(fold2(i))
    else if (i == 4) LineSegment(fold2(3), fold2(15)).reflect(fold2(i))
    else fold2(i)
  }

  def pointToString(p: Point): String = {
    val (xn, xd) = (p.x.numerator, p.x.denominator)
    val (yn, yd) = (p.y.numerator, p.y.denominator)
    val stringx = if (xd == 1) s"$xn" else s"$xn/$xd"
    val stringy = if (yd == 1) s"$yn" else s"$yn/$yd"
    s"$stringx,$stringy"
  }

  println(fold2(15))
  println(fold2(6))
  println(fold2(5))
  println(fold2(4))
  println("======")

  println(source.length)
  source.foreach(p => println(pointToString(p)))
  println("............")
  destination.foreach(p => println(pointToString(p)))

}

object ProblemMaker4 extends App {
  val source = Array(
    Point(0, 0),
    Point(1, 0),
    Point(1, 1),
    Point(0, 1),
    Point(3.0 / 4, 1.0 / 4)
  )

  val fold1 = (0 to 4).map { (i: Int) =>
    if (i == 0) LineSegment(source(1), source(3)).reflect(source(i))
    else source(i)
  }

  val fold2 = (0 to 4).map { (i: Int) =>
    if (i == 1) LineSegment(fold1(2), fold1(4)).reflect(fold1(i))
    else fold1(i)
  }

  val destination = fold2.map{(p: Point) =>
    val strangeNum: Int = 2048 * 32  // change this number to get a different variation.
    LineSegment(Point(-2, 1), Point(1.0/strangeNum, 0)).reflect(p)}

  def pointToString(p: Point): String = {
    val (xn, xd) = (p.x.numerator, p.x.denominator)
    val (yn, yd) = (p.y.numerator, p.y.denominator)
    val stringx = if (xd == 1) s"$xn" else s"$xn/$xd"
    val stringy = if (yd == 1) s"$yn" else s"$yn/$yd"
    s"$stringx,$stringy"
  }


  println(source.length)
  source.foreach(p => println(pointToString(p)))
  println("""4
3 0 1 4
3 1 2 4
3 2 3 4
3 0 4 3""")
  destination.foreach(p => println(pointToString(p)))

}
