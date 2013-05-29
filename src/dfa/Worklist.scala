package tapy.dfa

import tapy.cfg._
import tapy.dfa.MonotoneFrameworkTypes._
import org.python.indexer._;
import org.python.indexer.ast._;
import org.python.antlr._;
import org.python.antlr.base._;
import org.python.antlr.runtime._;
import org.python.antlr.runtime.tree._;
import java.io._
import tapy.export._
import tapy.lattices.AnalysisLattice
import tapy.typeanalysis.TypeAnalysis
import tapy.lattices.HeapLattice

class Worklist[T] (analysis: Analysis[T], lattice: Lattice[T], var cfg: ControlFlowGraph, path: String) {
  
  var oldCfg: ControlFlowGraph = cfg
  var newSubCfg: ControlFlowGraph = null
  
  def run(): T = {
    var constraints = cfg.nodes.foldLeft (Map(): ConstraintMap[T]) ((m, node) => m + (node -> (analysis.generateConstraint(node))))

    def visit (worklist: List[Node], solution: T): T = worklist match {
      case Nil => solution
      case node :: tl => {
        var worklist = tl
        val constraint = constraints(node)
        val newSolution = constraint(solution)
        
        // analysis.pp(node, newSolution)
        
        if (newSolution != solution)
          worklist = worklist ++ analysis.nodeDependencies(node, newSolution)

        if (cfg eq oldCfg) {
          visit (worklist, newSolution)
        } else {
          oldCfg = cfg
          constraints = newSubCfg.nodes.foldLeft(constraints) {(acc, node) => acc + (node -> analysis.generateConstraint(node)) }
          visit (newSubCfg.entryNodes.toList ++ worklist, newSolution)
        }
      }
    }

    analysis.worklist = this
    return visit (cfg.entryNodes.toList, lattice.bottom) // This only works for a forward analysis!
  }
  
  def getCFG(name: String): ControlFlowGraph = {
    val file = path + name + ".py"
    val parser: BaseParser = new BaseParser(new ANTLRFileStream(path + name + ".py"), path + name + ".py", "ascii")
    val ast = parser.parseModule()
    val cfg = ast.accept(new CFGGeneratorVisitor(name)).minify().normalize()
    if (cfg.exitNodes.size == 1) cfg else cfg.append(NoOpNode("Module Exit"))
  }
  
  def setCFG(cfg: ControlFlowGraph, newSubCfg: ControlFlowGraph): Unit = {
    this.cfg = cfg
    this.newSubCfg = newSubCfg
  }
}