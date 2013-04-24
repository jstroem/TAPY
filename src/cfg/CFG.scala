package tapy.cfg

import java.io._
import tapy.export._
import scala.collection.JavaConversions._
import java.util.IdentityHashMap

case class ControlFlowGraph(entryNodes: Set[Node],
                            regularExitNodes: Set[Node],
                            exceptionExitNodes: Set[Node],
                            nodes: Set[Node],
                            regularEdges: Map[Node, Set[Node]],
                            exceptionEdges: Map[Node, Set[Node]]) {

  def this(entryNodes: Set[Node], 
           regularExitNodes: Set[Node],
           nodes: Set[Node],
           regularEdges: Map[Node, Set[Node]]) = {

    this(entryNodes, 
         regularExitNodes, 
         Set[Node](),
         nodes,
         regularEdges,
         Map[Node, Set[Node]]())
  }



  /*
   * Renamed ~> Deprecated
   */

  @deprecated("exitNodes is renamed to regularExitNodes", "Apr 24")
  def exitNodes = regularExitNodes

  @deprecated("edges is renamed to regularEdges", "Apr 24")
  def edges = regularEdges

  @deprecated("getNodePredecessors is renamed to getRegularNodePredecessors", "Apr 24")
  def getNodePredecessors(node: Node): Set[Node] = {
    getRegularNodePredecessors(node)
  }

  @deprecated("getNodePredecessors is renamed to getRegularNodeSuccessors", "Apr 24")
  def getNodeSuccessors(node: Node): Set[Node] = {
    getRegularNodeSuccessors(node)
  }
  
  @deprecated("setExitNode is renamed to setRegularExitNode", "Apr 24")
  def setExitNode(node: Node): ControlFlowGraph = {
    setExitNodes(Set(node))
  }

  @deprecated("setExitNodes is renamed to setRegularExitNodes", "Apr 24")  
  def setExitNodes(newExitNodes: Set[Node]): ControlFlowGraph = {
    new ControlFlowGraph(entryNodes, newExitNodes, nodes, edges)
  }

  @deprecated("connectNodes is renamed to addRegularEdges", "Apr 24")
  def connectNodes(predecessors: Set[Node], successors: Set[Node]): ControlFlowGraph = {
    val newEdges = predecessors.foldLeft(edges) {(acc, pred) =>
      acc.get(pred) match {
        case Some(currentPredSuccessors) =>
          val newPredSuccessors = currentPredSuccessors ++ successors
          acc + (pred -> newPredSuccessors)
        case None => acc + (pred -> successors)
      }
    }
    return new ControlFlowGraph(entryNodes, exitNodes, nodes, newEdges)
  }

  @deprecated("connectNodes is renamed to addRegularEdges", "Apr 24")
  def connectNodes(pred: Node, succ: Node): ControlFlowGraph = {
    connectNodes(Set(pred), Set(succ))
  }

  @deprecated("connectNodes is renamed to addRegularEdges", "Apr 24")
  def connectNodes(pred: Node, succs: Set[Node]): ControlFlowGraph = {
    connectNodes(Set(pred), succs)
  }

  @deprecated("connectNodes is renamed to addRegularEdges", "Apr 24")
  def connectNodes(preds: Set[Node], succ: Node): ControlFlowGraph = {
    connectNodes(preds, Set(succ))
  }

  /*
   * Accessors
   */

  def getRegularNodePredecessors(node: Node): Set[Node] = {
    nodes.filter ((n) => getRegularNodeSuccessors(n).contains(node))
  }

  def getRegularNodeSuccessors(node: Node): Set[Node] = {
    regularEdges.getOrElse(node, Set())
  }

  def getExceptionNodePredecessors(node: Node): Set[Node] = {
    nodes.filter ((n) => getExceptionNodeSuccessors(n).contains(node))
  }

  def getExceptionNodeSuccessors(node: Node): Set[Node] = {
    exceptionEdges.getOrElse(node, Set())
  }

  def getAllPredecessors(node: Node): (Set[Node], Set[Node]) = {
    (getRegularNodePredecessors(node), getExceptionNodePredecessors(node))
  }

  def getAllSuccessors(node: Node): (Set[Node], Set[Node]) = {
    (getRegularNodeSuccessors(node), getExceptionNodeSuccessors(node))
  }

  /*
   * Manipulation - Nodes
   */

  def addNode(node: Node): ControlFlowGraph = {
    addNodes(Set(node))
  }

  def addNodes(newNodes: Set[Node]): ControlFlowGraph = {
    new ControlFlowGraph(entryNodes, regularExitNodes, exceptionExitNodes, nodes ++ newNodes, regularEdges, exceptionEdges)
  }
  
  def setEntryNode(node: Node): ControlFlowGraph = {
    setEntryNodes(Set(node))
  }
 
  def setEntryNodes(newEntryNodes: Set[Node]): ControlFlowGraph = {
    new ControlFlowGraph(newEntryNodes, regularExitNodes, exceptionExitNodes, nodes, regularEdges, exceptionEdges)
  }

  def setRegularExitNode(node: Node): ControlFlowGraph = {
    setRegularExitNodes(Set(node))
  }
    
  def setRegularExitNodes(newExitNodes: Set[Node]): ControlFlowGraph = {
    new ControlFlowGraph(entryNodes, newExitNodes, exceptionExitNodes, nodes, regularEdges, exceptionEdges)
  }

  def setExceptionExitNode(node: Node): ControlFlowGraph = {
    setExceptionExitNodes(Set(node))
  }

  def setExceptionExitNodes(newExitNodes: Set[Node]): ControlFlowGraph = {
    new ControlFlowGraph(entryNodes, regularExitNodes, newExitNodes, nodes, regularEdges, exceptionEdges)
  }

  def removeNode(node: Node): ControlFlowGraph = {
    val filteredEntryNodes = if (entryNodes.contains(node)) entryNodes - node ++ getRegularNodeSuccessors(node) else entryNodes
    val filteredRegularExitNodes = if (regularExitNodes.contains(node)) regularExitNodes - node ++ getRegularNodePredecessors(node) else regularExitNodes
    val filteredExceptionExitNodes = if (exceptionExitNodes.contains(node)) exceptionExitNodes - node ++ getExceptionNodePredecessors(node) else exceptionExitNodes
    val filteredNodes = nodes - node 
    val filteredRegularEdges = regularEdges.filterKeys{ (n) => node != n }.mapValues{ (ns) => ns - node }
    val filteredExceptionEdges = exceptionEdges.filterKeys{ (n) => node != n }.mapValues{ (ns) => ns - node }

    new ControlFlowGraph(filteredEntryNodes, 
                         filteredRegularExitNodes, 
                         filteredExceptionExitNodes, 
                         filteredNodes, 
                         filteredRegularEdges, 
                         filteredExceptionEdges).addRegularEdges(getRegularNodePredecessors(node), getRegularNodeSuccessors(node))
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

  def addRegularEdges(preds: Set[Node], succs: Set[Node]): ControlFlowGraph = {
    val newRegularEdges = addEdges(preds, succs, regularEdges)
    new ControlFlowGraph( entryNodes, regularExitNodes, exceptionExitNodes, nodes, newRegularEdges, exceptionEdges)
  }

  def addRegularEdges(pred: Node, succs: Set[Node]): ControlFlowGraph = { addRegularEdges(Set(pred), succs) }
  def addRegularEdges(preds: Set[Node], succ: Node): ControlFlowGraph = { addRegularEdges(preds, Set(succ)) }
  def addRegularEdges(pred: Node, succ: Node): ControlFlowGraph = { addRegularEdges(Set(pred), Set(succ)) }

  def addExceptionEdges(preds: Set[Node], succs: Set[Node]): ControlFlowGraph = {
    val newExceptionEdges = addEdges(preds, succs, exceptionEdges)
    new ControlFlowGraph( entryNodes, regularExitNodes, exceptionExitNodes, nodes, regularEdges, newExceptionEdges)
  }

  def addExceptionEdges(pred: Node, succs: Set[Node]): ControlFlowGraph = { addExceptionEdges(Set(pred), succs) }
  def addExceptionEdges(preds: Set[Node], succ: Node): ControlFlowGraph = { addExceptionEdges(preds, Set(succ)) }
  def addExceptionEdges(pred: Node, succ: Node): ControlFlowGraph = { addExceptionEdges(Set(pred), Set(succ)) }

  private def mapMerge[S, T] (a: Map[S, Set[T]], b: Map[S, Set[T]]) = {
    (a.keySet ++ b.keySet).foldLeft (Map[S, Set[T]]()) {(map, key) =>
      map + (key -> (a.getOrElse(key, Set[T]()) ++ b.getOrElse(key, Set[T]())))}
  }

  def combineGraphs(o: ControlFlowGraph): ControlFlowGraph = {
    //val newEdges = edges ++ o.edges.map {case (k, v) => k -> (v ++ edges.getOrElse(k, List()))}
    val newRegularEdges = mapMerge(regularEdges, o.regularEdges)
    val newExceptionEdges = mapMerge(exceptionEdges, o.exceptionEdges)
    new ControlFlowGraph(entryNodes ++ o.entryNodes, 
                         regularExitNodes ++ o.regularExitNodes,
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
      return node.toString().dropRight(38) + ")" + entryNodeStr + exitNodeStr // dropRight: Remove node id
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

object ControlFlowGraph {
  final val EMPTY = new ControlFlowGraph(Set(), Set(), Set(), Map())
  
  final def makeSingleton(node: Node): ControlFlowGraph = {
    val nodes = Set(node)
    return new ControlFlowGraph(nodes, nodes, nodes, Map())
  }
}
