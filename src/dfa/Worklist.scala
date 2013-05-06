package tapy.dfa

import tapy.cfg._
import tapy.dfa.MonotoneFrameworkTypes._

class WorklistAlgorithm[T] (analysis: Analysis[T],
                            lattice: Lattice[T],
                            graph: ControlFlowGraph) {

  def run(): Solution[T] = {
    val worklist = graph.nodes.toList
    val solution = graph.nodes.foldLeft (Map(): Solution[T]) ((m, node) => m + (node -> (lattice.bottom)))
    val contraints = graph.nodes.foldLeft (Map(): ConstraintMap[T]) ((m, node) => m + (node -> (analysis.generateConstraint(node))))

    def visit (worklist: List[Node], solution: Solution[T]): Solution[T] = worklist match {
      case Nil => solution
      case node::tl => {
        
        var worklist = tl
        val constraint: Constraint[T] = contraints(node)
        val newValue: T = constraint(solution)
        val currValue: T = solution(node)
        val newSolution: Map[Node, T] = solution + (node -> newValue)
       
        // TODO kan den teste det her ordenligt???
        if (currValue != newValue) {
          worklist = worklist ++ analysis.nodeDependencies(node)
        }

        visit (worklist, newSolution)
      }
    }

    return visit (worklist, solution)
  }
}
