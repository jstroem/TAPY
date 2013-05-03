package tapy.cfg

import java.io._
import tapy.export._
import scala.collection.JavaConversions._
import java.util.IdentityHashMap

case class ControlFlowGraph(entryNodes: Set[Node],
                            exitNodes: Set[Node],
                            exceptExitNodes: Set[Node],
                            nodes: Set[Node],
                            edges: Map[Node, Set[Node]],
                            exceptionEdges: Map[Node, Set[Node]]) {

  def this(node: Node) = { this(Set(node), Set(node), Set(), Set(node), Map(), Map()) }
  def this(nodes: Set[Node]) = { this(nodes, nodes, Set(), nodes, Map(), Map()) }

  /*
   * Accessors
   */

  def getPredecessors(node: Node): Set[Node] = {
    nodes.filter ((n) => getSuccessors(n).contains(node))
  }

  def getSuccessors(node: Node): Set[Node] = {
    edges.getOrElse(node, Set())
  }

  def getExceptionPredecessors(node: Node): Set[Node] = {
    nodes.filter ((n) => getExceptionSuccessors(n).contains(node))
  }

  def getExceptionSuccessors(node: Node): Set[Node] = {
    exceptionEdges.getOrElse(node, Set())
  }

  /*
   * Manipulation - Add nodes
   */

  def addNode(node: Node): ControlFlowGraph = {
    addNodes(Set(node))
  }

  def addNodes(newNodes: Set[Node]): ControlFlowGraph = {
    new ControlFlowGraph(entryNodes, exitNodes, exceptExitNodes, nodes ++ newNodes, edges, exceptionEdges)
  }

  /*
   * Manipulation - Add/set entry/exit nodes
   */

  def addExitNodes(newExitNodes: Set[Node]): ControlFlowGraph = {
    return new ControlFlowGraph(entryNodes, exitNodes ++ newExitNodes: Set[Node], exceptExitNodes, nodes, edges, exceptionEdges)
  }

  def addExceptExitNodes(newExceptExitNodes: Set[Node]): ControlFlowGraph = {
    return new ControlFlowGraph(entryNodes, exitNodes, exceptExitNodes ++ newExceptExitNodes, nodes, edges, exceptionEdges)
  }
  
  def setEntryNode(node: Node): ControlFlowGraph = {
    return setEntryNodes(Set(node))
  }
 
  def setEntryNodes(newEntryNodes: Set[Node]): ControlFlowGraph = {
    return new ControlFlowGraph(newEntryNodes, exitNodes, exceptExitNodes, nodes, edges, exceptionEdges)
  }
  
  def setExitNode(node: Node): ControlFlowGraph = {
    return setExitNodes(Set(node))
  }
    
  def setExitNodes(newExitNodes: Set[Node]): ControlFlowGraph = {
    return new ControlFlowGraph(entryNodes, newExitNodes, exceptExitNodes, nodes, edges, exceptionEdges)
  }
  
  def setExceptExitNode(node: Node): ControlFlowGraph = {
    return setExceptExitNodes(Set(node))
  }

  def setExceptExitNodes(newExitNodes: Set[Node]): ControlFlowGraph = {
    return new ControlFlowGraph(entryNodes, exitNodes, newExitNodes, nodes, edges, exceptionEdges)
  }

  /*
   * Manipulation
   */

  def removeNodeAndEdges(node: Node): ControlFlowGraph = {
    new ControlFlowGraph(entryNodes - node, exitNodes - node, exceptExitNodes - node, nodes - node, edges, exceptionEdges)
      .removeEdges(getPredecessors(node), node)
      .removeEdges(node, getSuccessors(node))
      .removeExceptEdges(getExceptionPredecessors(node), node)
      .removeExceptEdges(node, getExceptionSuccessors(node))
  }
  
  def removeNoOpNode(node: Node): ControlFlowGraph = node match {
    case n: NoOpNode => {
      if (entryNodes.contains(node) || exitNodes.contains(node) || exceptExitNodes.contains(node))
        // Not always working in this case, so don't remove node.
        // Problem: The exit-node may have outgoing edges (e.g. in case of loops).
        return this
      
      if (getExceptionSuccessors(node).exists({(n) => getPredecessors(n).isEmpty && getExceptionPredecessors(n).size == 1})) {
        return this
      }

      val filteredEntryNodes = entryNodes // if (entryNodes.contains(node)) entryNodes - node ++ getSuccessors(node) else entryNodes
      val filteredExitNodes = exitNodes // if (exitNodes.contains(node)) exitNodes - node ++ getPredecessors(node) else exitNodes
      val filteredExceptExitNodes = exceptExitNodes // if (exceptExitNodes.contains(node)) exceptExitNodes - node ++ getExceptionPredecessors(node) else exceptExitNodes
      val filteredNodes = nodes - node
      val filteredEdges = edges.filterKeys{(n) => node != n}.mapValues{(ns) => ns - node}
      val filteredExceptionEdges = exceptionEdges.filterKeys{(n) => node != n}.mapValues{(ns) => ns - node}

      new ControlFlowGraph(filteredEntryNodes, filteredExitNodes, filteredExceptExitNodes, filteredNodes, filteredEdges, filteredExceptionEdges)
        .connect(getPredecessors(node), getSuccessors(node))
        .connectExcept(getExceptionPredecessors(node), getSuccessors(node))
    }

    case _ => this
  }
  
  /*
   * Manipulation - Edges
   */

  private def addEdges(predecessors: Set[Node], successors: Set[Node], edges: Map[Node, Set[Node]]): Map[Node, Set[Node]] = {
    return predecessors.foldLeft(edges) {(edgeMap, pred) =>
      val currSuccs = edgeMap.getOrElse(pred, Set())
      edgeMap + (pred -> (currSuccs ++ successors))
    }
  }
  
  def removeEdges(pred: Node, succ: Node): ControlFlowGraph = removeEdges(Set(pred), Set(succ))
  def removeEdges(pred: Node, succs: Set[Node]): ControlFlowGraph = removeEdges(Set(pred), succs)
  def removeEdges(preds: Set[Node], succ: Node): ControlFlowGraph = removeEdges(preds, Set(succ))
  def removeEdges(preds: Set[Node], succs: Set[Node]): ControlFlowGraph = {
    val filteredEdges = preds.foldLeft(edges) {(acc, pred) =>
      val currSuccs = acc.getOrElse(pred, Set())
      acc + (pred -> (currSuccs -- succs))
    }
    return new ControlFlowGraph(entryNodes, exitNodes, exceptExitNodes, nodes, filteredEdges, exceptionEdges)
  }
  
  def removeExceptEdges(pred: Node, succ: Node): ControlFlowGraph = removeEdges(Set(pred), Set(succ))
  def removeExceptEdges(pred: Node, succs: Set[Node]): ControlFlowGraph = removeEdges(Set(pred), succs)
  def removeExceptEdges(preds: Set[Node], succ: Node): ControlFlowGraph = removeEdges(preds, Set(succ))
  def removeExceptEdges(preds: Set[Node], succs: Set[Node]): ControlFlowGraph = {
    val filteredExceptionEdges = preds.foldLeft(exceptionEdges) {(acc, pred) =>
      val currSuccs = acc.getOrElse(pred, Set())
      acc + (pred -> (currSuccs -- succs))
    }
    return new ControlFlowGraph(entryNodes, exitNodes, exceptExitNodes, nodes, edges, filteredExceptionEdges)
  }

  def connect(pred: Node, succ: Node): ControlFlowGraph = { return connect(Set(pred), Set(succ)) }
  def connect(pred: Node, succs: Set[Node]): ControlFlowGraph = { return connect(Set(pred), succs) }
  def connect(preds: Set[Node], succ: Node): ControlFlowGraph = { return connect(preds, Set(succ)) }
  def connect(preds: Set[Node], succs: Set[Node]): ControlFlowGraph = {
    val newRegularEdges = addEdges(preds, succs, edges)
    return new ControlFlowGraph(entryNodes, exitNodes, exceptExitNodes, nodes, newRegularEdges, exceptionEdges)
  }
  
  def connectExcept(pred: Node, succ: Node): ControlFlowGraph = { return connectExcept(Set(pred), Set(succ)) }
  def connectExcept(pred: Node, succs: Set[Node]): ControlFlowGraph = { return connectExcept(Set(pred), succs) }
  def connectExcept(preds: Set[Node], succ: Node): ControlFlowGraph = { return connectExcept(preds, Set(succ)) }
  def connectExcept(preds: Set[Node], succs: Set[Node], overwrite: Boolean = false): ControlFlowGraph = {
    // Only allow one exception edge for each node (in case trying to add more than one, it is ignored)
    val newExceptionEdges = preds.foldLeft(exceptionEdges) {(edgeMap, pred) =>
      edgeMap.get(pred) match {
        case Some(currSuccs) => if (currSuccs.size >= 1 && !overwrite) edgeMap else edgeMap + (pred -> succs)
        case None => edgeMap + (pred -> succs)
      }
    }
    return new ControlFlowGraph(entryNodes, exitNodes, exceptExitNodes, nodes, edges, newExceptionEdges)
  }
  
  def connectExcept(o: ControlFlowGraph): ControlFlowGraph = {
    return insert(o).connectExcept(nodes, o.entryNodes)
  }
  
  /*
   * Manipulation - CFGs
   */
  
  private def mapMerge[S, T] (a: Map[S, Set[T]], b: Map[S, Set[T]]) = {
    (a.keySet ++ b.keySet).foldLeft (Map[S, Set[T]]()) {(map, key) =>
      map + (key -> (a.getOrElse(key, Set[T]()) ++ b.getOrElse(key, Set[T]())))}
  }

  def combine(o: ControlFlowGraph): ControlFlowGraph = {
    val newRegularEdges = mapMerge(edges, o.edges)
    val newExceptionEdges = mapMerge(exceptionEdges, o.exceptionEdges)
    return new ControlFlowGraph(entryNodes ++ o.entryNodes, 
                         exitNodes ++ o.exitNodes,
                         exceptExitNodes ++ o.exceptExitNodes,
                         nodes ++ o.nodes,
                         newRegularEdges,
                         newExceptionEdges)
  }

  def combine(os: Set[ControlFlowGraph]): ControlFlowGraph = {
    return os.foldLeft(this) {(acc, o) => acc.combine(o)}
  }
  
  def append(node: Node): ControlFlowGraph = {
    return append(new ControlFlowGraph(node))
  }
  
  def append(o: ControlFlowGraph): ControlFlowGraph = {
    return combine(o).setEntryNodes(entryNodes).setExitNodes(o.exitNodes).connect(exitNodes, o.entryNodes)
  }
  
  def append(os: Set[ControlFlowGraph]): ControlFlowGraph = {
    return append(os.tail.foldLeft(os.head) {(acc, o) => acc.combine(o)})
  }
  
  /*
  def appendExcept(o: ControlFlowGraph): ControlFlowGraph = {
    insert(o).connectExcept(exitNodes, o.entryNodes)
  }
  */
  
  def insert(o: ControlFlowGraph): ControlFlowGraph = { return insert(o, Set[Node](), Set[Node]()) }
  def insert(o: ControlFlowGraph, pred: Node, succ: Node): ControlFlowGraph = { return insert(o, Set(pred), Set(succ)) }
  def insert(o: ControlFlowGraph, pred: Node, succs: Set[Node]): ControlFlowGraph = { return insert(o, Set(pred), succs) }
  def insert(o: ControlFlowGraph, preds: Set[Node], succ: Node): ControlFlowGraph = { return insert(o, preds, Set(succ)) }
  def insert(o: ControlFlowGraph, preds: Set[Node], succs: Set[Node]): ControlFlowGraph = {
    return combine(o).setEntryNodes(entryNodes).setExitNodes(exitNodes).connect(preds, o.entryNodes).connect(o.exitNodes, succs)
  }
  /*
  def insertExcept(o: ControlFlowGraph): ControlFlowGraph = { return insertExcept(o, Set[Node](), Set[Node]()) }
  def insertExcept(o: ControlFlowGraph, pred: Node, succ: Node): ControlFlowGraph = { return insertExcept(o, Set(pred), Set(succ)) }
  def insertExcept(o: ControlFlowGraph, pred: Node, succs: Set[Node]): ControlFlowGraph = { return insertExcept(o, Set(pred), succs) }
  def insertExcept(o: ControlFlowGraph, preds: Set[Node], succ: Node): ControlFlowGraph = { return insertExcept(o, preds, Set(succ)) }
  def insertExcept(o: ControlFlowGraph, preds: Set[Node], succs: Set[Node]): ControlFlowGraph = {
    return insert(o).connectExcept(preds, o.entryNodes).connectExcept(o.exitNodes, succs)
  }
  */
  
  // Removes all NoOpNodes
  def minify(): ControlFlowGraph = {
    val nodesToRemove = nodes.foldLeft(Set[Node]()) {(acc, node) =>
      node match {
        case node: NoOpNode => acc + node
        case node => acc
      }
    }
    return nodesToRemove.foldLeft(this) {(acc, node) => 
      acc.removeNoOpNode(node)
    }
  }
  
  def exportToFile(fileName: String, doCollapse : Boolean = true, doMinify: Boolean = true): ControlFlowGraph = {
    GraphvizExporter.export(generateGraphvizGraph(doCollapse), new PrintStream(fileName + ".cfg.dot"))
    Runtime.getRuntime().exec("dot -Tgif -o " + fileName + ".cfg.gif " + fileName + ".cfg.dot")
    
    if (doMinify) {
      GraphvizExporter.export(minify().generateGraphvizGraph(doCollapse), new PrintStream(fileName + ".cfg.min.dot"))
      Runtime.getRuntime().exec("dot -Tgif -o " + fileName + ".cfg.min.gif " + fileName + ".cfg.min.dot")
    }
  
    return this
  }
  
  def generateGraphvizGraph(collapse : Boolean) : GraphvizExporter.Graph = {
    def nodeToString(node: Node): String = {
      if (node == null)
          return "null"
      val entryNodeStr = if (entryNodes.contains(node)) "\nEntry node" else ""
      val exitNodeStr = if (exitNodes.contains(node)) "\nExit node" else ""
      val exitExceptNodeStr = if (exceptExitNodes.contains(node)) "\nExcept exit node" else ""
      return node.toString() + entryNodeStr + exitNodeStr + exitExceptNodeStr
    }

    var nodeMap = new IdentityHashMap[Node, GraphvizExporter.Node]()

    this.nodes.foreach((node) => nodeMap.put(node, GraphvizExporter.Node(nodeToString(node))))

    def getNodeId(node: Node) : String = nodeMap.get(node) match {
      case null => "" 
      case n => n.id
    }

    var graphNodes = nodeMap.values.toList

    var graphEdges = this.edges.foldLeft(List() : List[GraphvizExporter.Edge]) {(list, pair) =>
        var (from, toList) = pair
        toList.foldLeft(list)((list, to) => GraphvizExporter.Edge(getNodeId(from), getNodeId(to)) :: list)}

    var graphExceptEdges = this.exceptionEdges.foldLeft(List() : List[GraphvizExporter.Edge]) {(list, pair) => 
        var (from, toList) = pair
        toList.foldLeft(list)((list, to) => GraphvizExporter.Edge(getNodeId(from), getNodeId(to), None, Some(GraphvizExporter.Dashed())) :: list)}

    def collapseNodes(n : GraphvizExporter.Node, m: GraphvizExporter.Node, nodes: List[GraphvizExporter.Node], edges: List[GraphvizExporter.Edge], exceptEdges: List[GraphvizExporter.Edge]) : (List[GraphvizExporter.Node], List[GraphvizExporter.Edge], List[GraphvizExporter.Edge], Boolean) = {
      val outgoingEdges = GraphvizExporter.getOutgoingEdges(n, edges)
      if (outgoingEdges.size == 1 && outgoingEdges.head.to == m.id) {
        val ingoingEdges = GraphvizExporter.getIngoingEdges(m, edges)
        val ingoingExceptEdges = GraphvizExporter.getIngoingEdges(m, exceptEdges)
        if (ingoingEdges.size == 1 && ingoingEdges.head.from == n.id && ingoingExceptEdges.size == 0) {
          val exceptTargetsFromN = GraphvizExporter.getOutgoingEdges(n, exceptEdges).map((e) => e.to)
          val exceptTargetsFromM = GraphvizExporter.getOutgoingEdges(m, exceptEdges).map((e) => e.to)
          if (exceptTargetsFromN == exceptTargetsFromM) {
            val newNodes = (new GraphvizExporter.Node(n.label +"\n" + m.label, m.id)) :: nodes.filter((node) => node.id != n.id && node.id != m.id)
            val newEdges = edges.foldLeft(List() : List[GraphvizExporter.Edge])((acc,edge) => edge match {
              case GraphvizExporter.Edge(n.id, m.id, _, _) => acc
              case GraphvizExporter.Edge(e, n.id, _, _) => (new GraphvizExporter.Edge(e, m.id, edge.label, edge.style)) :: acc
              case _ => edge :: acc
            })
            val newExceptEdges = exceptEdges.foldLeft(List() : List[GraphvizExporter.Edge])((acc,edge) => edge match {
              case GraphvizExporter.Edge(n.id, m.id, _, _) => acc
              case GraphvizExporter.Edge(_, n.id, _, _) => (new GraphvizExporter.Edge(edge.from, m.id, edge.label, edge.style)) :: acc
              case GraphvizExporter.Edge(n.id, _, _, _) => acc
              case _ => edge :: acc
            })
            return (newNodes, newEdges, newExceptEdges, true)
          }
        }
      }
      return (nodes, edges, exceptEdges, false)
    } 

    def blockify(nodes: List[GraphvizExporter.Node], edges: List[GraphvizExporter.Edge], exceptEdges: List[GraphvizExporter.Edge]) : (List[GraphvizExporter.Node], List[GraphvizExporter.Edge], List[GraphvizExporter.Edge]) = {
      for (n <- nodes) {
        for (m <- nodes) {
          val (blockedNodes, blockedEdges, blockedExceptEdges,collapsed) = collapseNodes(n, m, nodes, edges, exceptEdges)
          if (collapsed) {
            return blockify(blockedNodes, blockedEdges, blockedExceptEdges)
          }
        }
      }
      return (nodes, edges, exceptEdges)
    }

    val (blockedNodes, blockedEdges, blockedExceptEdges) = if (collapse) blockify(graphNodes, graphEdges, graphExceptEdges) else (graphNodes, graphEdges, graphExceptEdges)

    return new GraphvizExporter.Graph {
      def nodes = blockedNodes
      def edges() = blockedEdges ::: blockedExceptEdges
      def subgraphs() = List()
      def name() = "ControlFlowGraph"
    }
  }
}
