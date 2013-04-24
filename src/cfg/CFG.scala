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

  def getNodePredecessors(node: Node): Set[Node] = {
    nodes.filter ((n) => getNodeSuccessors(n).contains(node))
  }

  def getNodeSuccessors(node: Node): Set[Node] = {
    edges.getOrElse(node, Set())
  }

  def getExceptionNodePredecessors(node: Node): Set[Node] = {
    nodes.filter ((n) => getExceptionNodeSuccessors(n).contains(node))
  }

  def getExceptionNodeSuccessors(node: Node): Set[Node] = {
    exceptionEdges.getOrElse(node, Set())
  }

  def getAllPredecessors(node: Node): (Set[Node], Set[Node]) = {
    (getNodePredecessors(node), getExceptionNodePredecessors(node))
  }

  def getAllSuccessors(node: Node): (Set[Node], Set[Node]) = {
    (getNodeSuccessors(node), getExceptionNodeSuccessors(node))
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
    setEntryNodes(Set(node))
  }
 
  def setEntryNodes(newEntryNodes: Set[Node]): ControlFlowGraph = {
    new ControlFlowGraph(newEntryNodes, exitNodes, exceptionExitNodes, nodes, edges, exceptionEdges)
  }

  def setExitNode(node: Node): ControlFlowGraph = {
    setExitNodes(Set(node))
  }
    
  def setExitNodes(newExitNodes: Set[Node]): ControlFlowGraph = {
    new ControlFlowGraph(entryNodes, newExitNodes, exceptionExitNodes, nodes, edges, exceptionEdges)
  }

  def setExceptionExitNode(node: Node): ControlFlowGraph = {
    setExceptionExitNodes(Set(node))
  }

  def setExceptionExitNodes(newExitNodes: Set[Node]): ControlFlowGraph = {
    new ControlFlowGraph(entryNodes, exitNodes, newExitNodes, nodes, edges, exceptionEdges)
  }

  def removeNode(node: Node): ControlFlowGraph = {
    val filteredEntryNodes = if (entryNodes.contains(node)) entryNodes - node ++ getNodeSuccessors(node) else entryNodes
    val filteredRegularExitNodes = if (exitNodes.contains(node)) exitNodes - node ++ getNodePredecessors(node) else exitNodes
    val filteredExceptionExitNodes = if (exceptionExitNodes.contains(node)) exceptionExitNodes - node ++ getExceptionNodePredecessors(node) else exceptionExitNodes
    val filteredNodes = nodes - node 
    val filteredRegularEdges = edges.filterKeys{ (n) => node != n }.mapValues{ (ns) => ns - node }
    val filteredExceptionEdges = exceptionEdges.filterKeys{ (n) => node != n }.mapValues{ (ns) => ns - node }

    new ControlFlowGraph(filteredEntryNodes, 
                         filteredRegularExitNodes, 
                         filteredExceptionExitNodes, 
                         filteredNodes, 
                         filteredRegularEdges, 
                         filteredExceptionEdges)
      .connectNodes(getNodePredecessors(node), getNodeSuccessors(node))
      .addExceptionEdges(getExceptionNodePredecessors(node), getExceptionNodeSuccessors(node))
  }
  
  /*
   * Manipulation - Edges
   */

  private def addEdges(predecessors: Set[Node], successors: Set[Node], edges: Map[Node, Set[Node]]): Map[Node, Set[Node]] = {
    predecessors.foldLeft(edges) {(edgeMap, pred) =>
      val currSuccs = edgeMap.getOrElse(pred, Set())
      edgeMap + (pred -> (currSuccs ++ successors))}
  }

  def connectNodes(preds: Set[Node], succs: Set[Node]): ControlFlowGraph = {
    val newRegularEdges = addEdges(preds, succs, edges)
    new ControlFlowGraph( entryNodes, exitNodes, exceptionExitNodes, nodes, newRegularEdges, exceptionEdges)
  }

  def connectNodes(pred: Node, succs: Set[Node]): ControlFlowGraph = { connectNodes(Set(pred), succs) }
  def connectNodes(preds: Set[Node], succ: Node): ControlFlowGraph = { connectNodes(preds, Set(succ)) }
  def connectNodes(pred: Node, succ: Node): ControlFlowGraph = { connectNodes(Set(pred), Set(succ)) }

  def addExceptionEdges(preds: Set[Node], succs: Set[Node]): ControlFlowGraph = {
    val newExceptionEdges = addEdges(preds, succs, exceptionEdges)
    new ControlFlowGraph( entryNodes, exitNodes, exceptionExitNodes, nodes, edges, newExceptionEdges)
  }

  def addExceptionEdges(pred: Node, succs: Set[Node]): ControlFlowGraph = { addExceptionEdges(Set(pred), succs) }
  def addExceptionEdges(preds: Set[Node], succ: Node): ControlFlowGraph = { addExceptionEdges(preds, Set(succ)) }
  def addExceptionEdges(pred: Node, succ: Node): ControlFlowGraph = { addExceptionEdges(Set(pred), Set(succ)) }

  private def mapMerge[S, T] (a: Map[S, Set[T]], b: Map[S, Set[T]]) = {
    (a.keySet ++ b.keySet).foldLeft (Map[S, Set[T]]()) {(map, key) =>
      map + (key -> (a.getOrElse(key, Set[T]()) ++ b.getOrElse(key, Set[T]())))}
  }

  def combineGraphs(o: ControlFlowGraph): ControlFlowGraph = {
    val newRegularEdges = mapMerge(edges, o.edges)
    val newExceptionEdges = mapMerge(exceptionEdges, o.exceptionEdges)
    new ControlFlowGraph(entryNodes ++ o.entryNodes, 
                         exitNodes ++ o.exitNodes,
                         exceptionExitNodes ++ o.exceptionExitNodes,
                         nodes ++ o.nodes,
                         newRegularEdges,
                         newExceptionEdges)
  }

  def combineGraphsSpecialized(o: ControlFlowGraph): ControlFlowGraph = {
    val newRegularEdges = mapMerge(edges, o.edges)
    val newExceptionEdges = mapMerge(exceptionEdges, o.exceptionEdges)
    new ControlFlowGraph(entryNodes, 
                         o.exitNodes,
                         exceptionExitNodes ++ o.exceptionExitNodes,
                         nodes ++ o.nodes,
                         newRegularEdges,
                         newExceptionEdges)
  }
  
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
  
  def exportToFile(fileName: String): ControlFlowGraph = {
    GraphvizExporter.export(generateGraphvizGraph(), new PrintStream(fileName + ".cfg.dot"))
    Runtime.getRuntime().exec("dot -Tgif -o " + fileName + ".cfg.gif " + fileName + ".cfg.dot")
    
    GraphvizExporter.export(minify().generateGraphvizGraph(), new PrintStream(fileName + ".cfg.min.dot"))
    Runtime.getRuntime().exec("dot -Tgif -o " + fileName + ".cfg.min.gif " + fileName + ".cfg.min.dot")
    
    return this
  }
  
  def generateGraphvizGraph() : GraphvizExporter.Graph = {
    def nodeToString(node: Node): String = {
      if (node == null)
          return "null"
      val entryNodeStr = if (entryNodes.contains(node)) "\nEntry node" else ""
      val exitNodeStr = if (exitNodes.contains(node)) "\nExit node" else ""
      return node.toString() + entryNodeStr + exitNodeStr // dropRight: Remove node id
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

    return new GraphvizExporter.Graph {
      def nodes = graphNodes
      def edges() = graphEdges
      def subgraphs() = List()
      def name() = "ControlFlowGraph"
    }
  }
}