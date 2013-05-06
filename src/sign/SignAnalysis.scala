package tapy.sign

import tapy.dfa._
import tapy.cfg._
import tapy.dfa.MonotoneFrameworkTypes._
import tapy.constants.BinOp._

abstract class SignAnalysis (graph: ControlFlowGraph) extends Analysis[Sign] {

  def generateConstraint(cfgNode: Node): Constraint[Sign] = {
    return null
  }

  def nodeDependencies (cfgNode: Node): Set[Node] = {
    graph.getPredecessors(cfgNode)
  }



  private def signConstant(i: Int) = {
    if (i == 0) Zero()
    else if (i > 0) Plus()
    else Minus()
  }

  private def abstractBinop(a: Sign, b: Sign, binop: BinOp): Sign = binop match {
    case PLUS => (a, b) match {
      case (Bottom(), _) => Bottom()
      case (_, Bottom()) => Bottom()
      case (QuestionMark(), _) => QuestionMark()
      case (_, QuestionMark()) => QuestionMark()

      case (Plus(), Plus())  => Plus()
      case (Plus(), Minus()) => QuestionMark()
      case (Plus(), Zero())  => Plus()

      case (Minus(), Plus())  => QuestionMark()
      case (Minus(), Minus()) => Minus()
      case (Minus(), Zero())  => Minus()

      case (Zero(), Plus())   => Plus()
      case (Zero(), Minus())  => Minus()
      case (Zero(), Zero())   => Zero()
    }
    case _ => throw new UnsupportedOperationException()
  }
}





