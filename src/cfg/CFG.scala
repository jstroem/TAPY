package tapy.cfg

import java.io._
import tapy.export._
import scala.collection.JavaConversions._
import java.util.IdentityHashMap

case class ControlFlowGraph(
    entryNodes: Set[Node],
    exitNodes: Set[Node],
    nodes: Set[Node],
    edges: Map[Node, Set[Node]]) {
  
  def getNodePredecessors(node: Node): Set[Node] = {
    return edges.foldLeft(Set[Node]()) {(acc, entry) => if (entry._2.contains(node)) acc + entry._1 else acc}
  }

  def getNodeSuccessors(node: Node): Set[Node] = {
    return edges.get(node) match {
      case Some(successors) => successors
      case None => Set()
    }
  }
  
  def addNode(node: Node): ControlFlowGraph = {
    return new ControlFlowGraph(entryNodes, exitNodes, nodes + node, edges)
  }
  
  def addNodes(newNodes: List[Node]): ControlFlowGraph = {
    return new ControlFlowGraph(entryNodes, exitNodes, nodes ++ newNodes, edges)
  }
  
  def setEntryNode(node: Node): ControlFlowGraph = {
    return new ControlFlowGraph(Set(node), exitNodes, nodes, edges)
  }
  
  def setEntryNodes(newEntryNodes: Set[Node]): ControlFlowGraph = {
    return new ControlFlowGraph(newEntryNodes, exitNodes, nodes, edges)
  }
  
  def setExitNode(node: Node): ControlFlowGraph = {
    return new ControlFlowGraph(entryNodes, Set(node), nodes, edges)
  }
  
  def setExitNodes(newExitNodes: Set[Node]): ControlFlowGraph = {
    return new ControlFlowGraph(entryNodes, newExitNodes, nodes, edges)
  }
                  
  def combineGraphs(o: ControlFlowGraph): ControlFlowGraph = {
    val newEdges = edges ++ o.edges.map {case (k, v) => k -> (v ++ edges.getOrElse(k, List()))}
    return new ControlFlowGraph(entryNodes ++ o.entryNodes, exitNodes ++ o.exitNodes, nodes ++ o.nodes, newEdges)
  }

  def connectNodes(pred: Node, succ: Node): ControlFlowGraph = {
    val newPredSuccessors = edges.get(pred) match {
      case Some(succs) => succs + succ
      case None => Set(succ)
    }
    return new ControlFlowGraph(entryNodes, exitNodes, nodes, edges + (pred -> newPredSuccessors))
  }

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

  def connectNodes(pred: Node, succs: Set[Node]): ControlFlowGraph = {
    return connectNodes(Set(pred), succs)
  }

  def connectNodes(preds: Set[Node], succ: Node): ControlFlowGraph = {
    return connectNodes(preds, Set(succ))
  }

  /**
    * Notice that this method does not add a new entry node, if the last entry node is removed.
    * The same holds for the exit node.
    */
  def removeNode(node: Node): ControlFlowGraph = {
    val filteredEntryNodes = entryNodes.filter({(startNode) => startNode != node})
    val filteredExitNodes = exitNodes.filter({(exitNode) => exitNode != node})
    val filteredNodes = nodes.filter({(otherNode) => otherNode != node})
    val filteredEdges = edges.foldLeft(Map[Node, Set[Node]]()) {(acc, entry) => if (entry._1 == node) acc else acc + (entry._1 -> entry._2.filter({(succ) => succ != node}))}
    return new ControlFlowGraph(filteredEntryNodes, filteredExitNodes, filteredNodes, filteredEdges).connectNodes(getNodePredecessors(node), getNodeSuccessors(node))
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