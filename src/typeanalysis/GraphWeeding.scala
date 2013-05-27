package tapy.typeanalysis

import tapy.cfg._

object GraphFix {
  private def reachable(n: Node, g: ControlFlowGraph): Set[Node] = reachable(n, g, Set[Node]())
  private def reachable(n: Node, g: ControlFlowGraph, seen: Set[Node]): Set[Node] = {
    if (seen.contains(n))
      return seen

    return (g.getSuccessors(n) ++ g.getExceptionSuccessors(n)).foldLeft(Set[Node]()) ((acc, node) => {
      acc ++ reachable(node, g, (seen+node))
    })
  }

  def moveGlobal(g: ControlFlowGraph): ControlFlowGraph = {
    val entries = g.nodes.foldLeft (Set[Node]()) {(acc, n) => n match {
      case n: FunctionEntryNode => acc + n
      case n: ModuleEntryNode => acc + n
      case n: ClassEntryNode => acc + n
      case _ => acc
    }}

    def globalFilter: (Node => Boolean) = ((node) => node match {
      case n: GlobalNode => true
      case _ => false
    })

    entries.foldLeft (g) ((graph, entry) => {
      val globals = reachable(entry, graph).filter(globalFilter)

      globals.foldLeft(graph) ((cfg, global) => {
        val normalSucc = cfg.getSuccessors(entry)
        val exceptSucc = cfg.getExceptionSuccessors(entry)
          cfg.removeNodeAndEdges(global)
             .removeEdges(entry, normalSucc)
             .removeExceptEdges(entry, exceptSucc)
             .connect(entry, global)
             .connect(global, normalSucc)
             .connectExcept(global, exceptSucc)
      })
    })
  }
}
