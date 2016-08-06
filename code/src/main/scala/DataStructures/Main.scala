package DataStructures

import cats.Monad
import cats.data.{StateT,State}

/**
  * Created by greddy on 8/5/16.
  */
object Main extends App {

  trait AdjEdgeT {
    val edge: EdgeT
    val neighbors: Seq[(EdgeT,FractionT)]
  }

  case class AdjEdge(edge:EdgeT,neighbors:Seq[(EdgeT,FractionT)]) extends AdjEdgeT

  type GState = Seq[AdjEdge]

  def getState(problem: ProblemT):GState = {
    val ske = problem.skeleton
    ske.edges.map(x => AdjEdge(x,x.neighbors(ske.edges)))
  }

  /*
  def getOuterEdges()
  def getInnerEdges()
  **/

  /*
  type Solve[A] = StateT[cats.Eval,GState,A]

  val solver:Solve[SolutionT] = State(generateState)
  */

  def solve(solution: SolutionT,problemT: ProblemT): Option[SolutionT] = {

    None
  }

  def compute(problem: ProblemT): SolutionT = {
    ???
  }


  def generateState(state:GState):(GState,SolutionT) = {
    val edge = getFoldableEdge(state)
    val newState = unfold(state, edge)
    ???
  }

  def isLegal(state:GState):Boolean = {
    true
  }

  def getFoldableEdge(state:GState):EdgeT = {
    state.head.edge
  }

  def unfold(state:GState, edgeT: EdgeT):GState = {
    state
  }

}
