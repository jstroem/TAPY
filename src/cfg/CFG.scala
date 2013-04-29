package tapy.cfg

import java.io._
import tapy.export._
import scala.collection.JavaConversions._
import java.util.IdentityHashMap

case class ControlFlowGraph(entryNodes: Set[Node],
                            exitNodes: Set[Node],
                            exceptionExitNodes: Set[Node],
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
   * Manipulation - Nodes
   */

  def addNode(node: Node): ControlFlowGraph = {
    addNodes(Set(node))
  }

  def addNodes(newNodes: Set[Node]): ControlFlowGraph = {
    new ControlFlowGraph(entryNodes, exitNodes, exceptionExitNodes, nodes ++ newNodes, edges, exceptionEdges)
  }
  
  def setEntryNode(node: Node): ControlFlowGraph = {
    return setEntryNodes(Set(node))
  }
 
  def setEntryNodes(newEntryNodes: Set[Node]): ControlFlowGraph = {
    return new ControlFlowGraph(newEntryNodes, exitNodes, exceptionExitNodes, nodes, edges, exceptionEdges)
  }

  def addExitNodes(newExitNodes: Set[Node]): ControlFlowGraph = {
    return new ControlFlowGraph(entryNodes, exitNodes ++ newExitNodes: Set[Node], exceptionExitNodes, nodes, edges, exceptionEdges)
  }
  
  def setExitNode(node: Node): ControlFlowGraph = {
    return setExitNodes(Set(node))
  }
    
  def setExitNodes(newExitNodes: Set[Node]): ControlFlowGraph = {
    return new ControlFlowGraph(entryNodes, newExitNodes, exceptionExitNodes, nodes, edges, exceptionEdges)
  }

  def setExceptionExitNode(node: Node): ControlFlowGraph = {
    return setExceptionExitNodes(Set(node))
  }

  def setExceptionExitNodes(newExitNodes: Set[Node]): ControlFlowGraph = {
    return new ControlFlowGraph(entryNodes, exitNodes, newExitNodes, nodes, edges, exceptionEdges)
  }

  def removeNode(node: Node): ControlFlowGraph = {
    if (entryNodes.contains(node) || exitNodes.contains(node) || exceptionExitNodes.contains(node))
      // Not always working in this case, so don't remove node.
      // Problem: The exit-node may have outgoing edges (e.g. in case of loops).
      return this 
    
    val filteredEntryNodes = entryNodes // if (entryNodes.contains(node)) entryNodes - node ++ getSuccessors(node) else entryNodes
    val filteredExitNodes = exitNodes // if (exitNodes.contains(node)) exitNodes - node ++ getPredecessors(node) else exitNodes
    val filteredExceptionExitNodes = exceptionExitNodes // if (exceptionExitNodes.contains(node)) exceptionExitNodes - node ++ getExceptionPredecessors(node) else exceptionExitNodes
    val filteredNodes = nodes - node 
    val filteredEdges = edges.filterKeys{(n) => node != n}.mapValues{(ns) => ns - node}
    val filteredExceptionEdges = exceptionEdges.filterKeys{(n) => node != n}.mapValues{(ns) => ns - node}

    new ControlFlowGraph(filteredEntryNodes, filteredExitNodes, filteredExceptionExitNodes, filteredNodes, filteredEdges, filteredExceptionEdges)
      .connect(getPredecessors(node), getSuccessors(node))
      .connectExcept(getPredecessors(node), getExceptionSuccessors(node))
      .connectExcept(getExceptionPredecessors(node), getExceptionSuccessors(node))
      .connectExcept(getExceptionPredecessors(node), getSuccessors(node))
  }
  
  /*
   * Manipulation - Edges
   */

  private def addEdges(predecessors: Set[Node], successors: Set[Node], edges: Map[Node, Set[Node]]): Map[Node, Set[Node]] = {
    predecessors.foldLeft(edges) {(edgeMap, pred) =>
      val currSuccs = edgeMap.getOrElse(pred, Set())
      edgeMap + (pred -> (currSuccs ++ successors))}
  }

  def connect(preds: Set[Node], succs: Set[Node]): ControlFlowGraph = {
    val newRegularEdges = addEdges(preds, succs, edges)
    new ControlFlowGraph(entryNodes, exitNodes, exceptionExitNodes, nodes, newRegularEdges, exceptionEdges)
  }

  def connect(pred: Node, succs: Set[Node]): ControlFlowGraph = { connect(Set(pred), succs) }
  def connect(preds: Set[Node], succ: Node): ControlFlowGraph = { connect(preds, Set(succ)) }
  def connect(pred: Node, succ: Node): ControlFlowGraph = { connect(Set(pred), Set(succ)) }


  def connectExcept(pred: Node, succ: Node): ControlFlowGraph = { connectExcept(Set(pred), Set(succ)) }
  def connectExcept(pred: Node, succs: Set[Node]): ControlFlowGraph = { connectExcept(Set(pred), succs) }
  def connectExcept(preds: Set[Node], succ: Node): ControlFlowGraph = { connectExcept(preds, Set(succ)) }
  def connectExcept(preds: Set[Node], succs: Set[Node]): ControlFlowGraph = {
    val newExceptionEdges = addEdges(preds, succs, exceptionEdges)
    new ControlFlowGraph(entryNodes, exitNodes, exceptionExitNodes, nodes, edges, newExceptionEdges)
  }
  
  def connectExcept(o: ControlFlowGraph): ControlFlowGraph = {
    insert(o).connectExcept(nodes, o.entryNodes)
  }
  
  private def mapMerge[S, T] (a: Map[S, Set[T]], b: Map[S, Set[T]]) = {
    (a.keySet ++ b.keySet).foldLeft (Map[S, Set[T]]()) {(map, key) =>
      map + (key -> (a.getOrElse(key, Set[T]()) ++ b.getOrElse(key, Set[T]())))}
  }

  def combine(o: ControlFlowGraph): ControlFlowGraph = {
    val newRegularEdges = mapMerge(edges, o.edges)
    val newExceptionEdges = mapMerge(exceptionEdges, o.exceptionEdges)
    return new ControlFlowGraph(entryNodes ++ o.entryNodes, 
                         exitNodes ++ o.exitNodes,
                         exceptionExitNodes ++ o.exceptionExitNodes,
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
    return nodesToRemove.foldLeft(this) {(acc, node) => acc.removeNode(node)}
  }
  
  def exportToFile(fileName: String, doMinify: Boolean = true): ControlFlowGraph = {
    GraphvizExporter.export(generateGraphvizGraph(), new PrintStream(fileName + ".cfg.dot"))
    Runtime.getRuntime().exec("dot -Tgif -o " + fileName + ".cfg.gif " + fileName + ".cfg.dot")
    
    if (doMinify) {
      GraphvizExporter.export(minify().generateGraphvizGraph(), new PrintStream(fileName + ".cfg.min.dot"))
      Runtime.getRuntime().exec("dot -Tgif -o " + fileName + ".cfg.min.gif " + fileName + ".cfg.min.dot")
    }
  
    return this
  }
  
  def generateGraphvizGraph() : GraphvizExporter.Graph = {
    def nodeToString(node: Node): String = {
      if (node == null)
          return "null"
      val entryNodeStr = if (entryNodes.contains(node)) "\nEntry node" else ""
      val exitNodeStr = if (exitNodes.contains(node)) "\nExit node" else ""
      val exitExceptNodeStr = if (exceptionExitNodes.contains(node)) "\nExcept exit node" else ""
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

    var graphExceptEdges = this.exceptionEdges.foldLeft(graphEdges) {(list, pair) => 
        var (from, toList) = pair
        toList.foldLeft(list)((list, to) => GraphvizExporter.Edge(getNodeId(from), getNodeId(to), None, Some(GraphvizExporter.Dashed())) :: list)}

    return new GraphvizExporter.Graph {
      def nodes = graphNodes
      def edges() = graphExceptEdges
      def subgraphs() = List()
      def name() = "ControlFlowGraph"
    }
  }
}