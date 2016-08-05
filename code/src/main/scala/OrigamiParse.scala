import cats.Eval
import cats.data.{State, StateT}

import scala.math.BigInt
import Origami._

object OrigamiParse {
  type Parse[A] = StateT[cats.Eval, List[String], A]

  def tokenize(str: String): List[String] = str.split("""(\s|,)""").toList

  def repeat[A](n: Int)(p: Parse[A]): Parse[List[A]] = 
    if (n==0) State.pure(Nil)
    else p flatMap { a => repeat(n-1)(p) map (a :: _) }

  val parseNum: Parse[BigInt] =
    State { (toks: List[String]) =>
      (toks.tail, BigInt(toks.head.toInt))
    }

  def runParseRational(toks: List[String]): (List[String], Rational) = {
    val tok = toks.head
    (toks.tail,
      if (tok contains '/') {
        val Array(num, denom) = tok.split('/')
        new Rational(BigInt(num), BigInt(denom))
      } else {
        new Rational(BigInt(tok), BigInt(1))
      }
    )
  }
  val parseRational: Parse[Rational] = State(runParseRational)
  val parsePoint: Parse[Point] = repeat(2)(parseRational) map (Point(_:_*))
  val parseLineSegment: Parse[LineSegment] = repeat(2)(parsePoint) map {
    case p1 :: p2 :: Nil => (p1, p2)
  }

  val parsePolygon: Parse[Polygon] =
    parseNum flatMap {
      n => repeat(n.toInt)(parsePoint) map (Polygon(_))
    }

  val parseSilhouette: Parse[Silhouette] =
    parseNum flatMap {
      n => repeat(n.toInt)(parsePolygon) map (Silhouette(_))
    }

  val parseSkeleton: Parse[Skeleton] =
    parseNum flatMap {
      n => repeat(n.toInt)(parseLineSegment) map (Skeleton(_))
    }

  val parseProblem: Parse[Problem] =
    parseSilhouette flatMap { sil => parseSkeleton map { skel => (sil, skel) } }

}

object OrigamiParseExample extends App {

  import OrigamiParse._

  val ex =
    """1
4
0,0
1,0
1/2,1/2
0,1/2
5
0,0 1,0
1,0 1/2,1/2
1/2,1/2 0,1/2
0,1/2 0,0
0,0 1/2,1/2
"""

  val initToks = tokenize(ex)
  println(s"initToks = $initToks")
  val prob = parseProblem.run(initToks).value._2
  println(prob)


}
