package tapy.typeanalysis

import tapy.cfg._

trait Environment {
  var environment: Map[Node, Set[String]]
  
  object Environment {
    def build(g: ControlFlowGraph): Map[Node, Set[String]] = {
      def getVarName = {(n: Node) => n match {
        case WriteVariableNode(s,_,_) => s
        case FunctionDeclNode(entry, _, _, _) => entry.funcDef.getInternalName()
        case ClassDeclNode(entry, _, _, _) => entry.classDef.getInternalName()
        case _ => ""
      }}
   
      val entries = g.nodes.foldLeft (Set[Node]()) {(acc, n) => n match {
        case n: FunctionEntryNode => acc + n
        case n: ModuleEntryNode => acc + n
        case n: ClassEntryNode => acc + n
        case _ => acc
      }}
  
      entries.foldLeft (Map[Node, Set[String]]()) ({(acc, n) =>
        val vars = reachable(n, g).map(getVarName) - ""
        acc + (n -> vars)
      })
    }
  
    private def reachable(n: Node, g: ControlFlowGraph): Set[Node] = reachable(n, g, Set[Node]())
    private def reachable(n: Node, g: ControlFlowGraph, seen: Set[Node]): Set[Node] = {
      if (seen.contains(n))
        return seen
      
      val nsucc = g.getSuccessors(n).foldLeft(Set[Node]()) ((acc, node) => node match {
        case node: ModuleEntryNode =>
          // A new module begins, don't take the environment of that module
          acc
          
        case node: ClassEntryNode =>
          // A class is entered, don't take the environment of the class, but
          // continue taking the environment from where the class ends
          acc + getClassExitNode(node, g)
          
        case node: ExitNode =>
          // A function or class ends: Don't take the outer environment
          acc
        
        case _ =>
          acc + node
      })
      
      nsucc.foldLeft (seen+n) ({(acc, s) =>
        reachable(s, g, acc)
      })
    }
    
    private def getClassExitNode(node: Node, g: ControlFlowGraph, depth: Int = 0): Node = {
      val succs = g.getSuccessors(node)
      
      val res = succs.foldLeft(Set[Node]()) ((acc, succ) => succ match {
        case succ: ExitNode =>
          if (depth == 0) acc + succ else acc + getClassExitNode(succ, g, depth - 1)
          
        case succ: ClassEntryNode =>
          acc + getClassExitNode(succ, g, depth + 1)
          
        case succ =>
          acc + getClassExitNode(succ, g, depth)
      })
      
      if (res.size == 1)
        println("Success: " + res.head)
      else
        println("Error: " + res)
      
      res.head
    }
  }
}
