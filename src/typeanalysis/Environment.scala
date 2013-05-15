import tapy.cfg._

object Environment {
  def getEnvironments(g: ControlFlowGraph): Map[Node, Set[String]] = {
    def getVarName = {(n: Node) => n match {
      case WriteVariableNode(s,_,_) => s
      case _ => ""
    }}

    g.entryNodes.foldLeft (Map[Node, Set[String]]()) ({(acc, n) =>
      val vars = reachable(n, g).map(getVarName) - ""
      acc + (n -> vars)
    })
  }

  private def reachable(n: Node, g: ControlFlowGraph): Set[Node] = reachable(n, g, Set[Node]())
  private def reachable(n: Node, g: ControlFlowGraph, seen: Set[Node]): Set[Node] = {
    if (seen.contains(n))
      return seen
    
    val nsucc = g.getSuccessors(n)
    nsucc.foldLeft (seen+n) ({(acc, s) =>
      reachable(s, g, acc)
    })
  }
}
