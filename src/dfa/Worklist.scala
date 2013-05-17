package tapy.dfa

import tapy.cfg._
import tapy.dfa.MonotoneFrameworkTypes._

class Worklist[T] (analysis: Analysis[T], lattice: Lattice[T], graph: ControlFlowGraph) {

  def run(): T = {
    val worklist = graph.entryNodes.toList // graph.nodes.toList
    val solution = lattice.bottom
    val contraints = graph.nodes.foldLeft (Map(): ConstraintMap[T]) ((m, node) => m + (node -> (analysis.generateConstraint(node))))

    def visit (worklist: List[Node], solution: T): T = worklist match {
      case Nil => solution
      case node :: tl => {
        var worklist = tl
        val constraint = contraints(node)
        val newSolution = constraint(solution)
        
        // analysis.pp(node, newSolution)
        
        if (newSolution != solution) {
          worklist = worklist ++ analysis.nodeDependencies(node, newSolution)
        }

        visit (worklist, newSolution)
      }
    }

    return visit (worklist, solution)
  }
}