package tapy.cfg

import tapy.export._

case class ControlFlowGraph(
    entryNodes: List[Node],
    exitNodes: List[Node],
    nodes: List[Node],
    edges: Map[Node, List[Node]]) {
  
  def getNodePredecessors(node: Node): List[Node] = {
    return edges.foldLeft(List[Node]()) {(acc, entry) => if (entry._2.contains(node)) entry._1 :: acc else acc}
  }

  def getNodeSuccessors(node: Node): List[Node] = {
    return edges.get(node) match {
      case Some(successors) => successors
      case None => List[Node]()
    }
  }
  
  def addNode(node: Node): ControlFlowGraph = {
    return new ControlFlowGraph(entryNodes, exitNodes, node :: nodes, edges)
  }
  
  def addNodes(newNodes: List[Node]): ControlFlowGraph = {
    return new ControlFlowGraph(entryNodes, exitNodes, nodes ++ newNodes, edges)
  }
  
  def setEntryNode(node: Node): ControlFlowGraph = {
    return new ControlFlowGraph(node :: List(), exitNodes, nodes, edges)
  }
  
  def setEntryNodes(newEntryNodes: List[Node]): ControlFlowGraph = {
    return new ControlFlowGraph(newEntryNodes, exitNodes, nodes, edges)
  }
  
  def setExitNode(node: Node): ControlFlowGraph = {
    return new ControlFlowGraph(entryNodes, node :: List(), nodes, edges)
  }
  
  def setExitNodes(newExitNodes: List[Node]): ControlFlowGraph = {
    return new ControlFlowGraph(entryNodes, newExitNodes, nodes, edges)
  }
                  
  def combineGraphs(o: ControlFlowGraph): ControlFlowGraph = {
    return new ControlFlowGraph(entryNodes ++ o.entryNodes, exitNodes ++ o.exitNodes, nodes ++ o.nodes, edges ++ o.edges)
  }

  def connectNodes(pred: Node, succ: Node): ControlFlowGraph = {
    val newPredSuccessors = edges.get(pred) match {
      case Some(succs) => succ :: succs
      case None => succ :: List[Node]()
    }
    return new ControlFlowGraph(entryNodes, exitNodes, nodes, edges + (pred -> newPredSuccessors))
  }

  def connectNodes(predecessors: List[Node], successors: List[Node]): ControlFlowGraph = {
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

  def connectNodes(pred: Node, succs: List[Node]): ControlFlowGraph = {
    return connectNodes(pred :: List(), succs)
  }

  def connectNodes(preds: List[Node], succ: Node): ControlFlowGraph = {
    return connectNodes(preds, succ :: List())
  }

  /**
    * Notice that this method does not add a new entry node, if the last entry node is removed.
    * The same holds for the exit node.
    */
  def removeNode(node: Node): ControlFlowGraph = {
    val filteredEntryNodes = entryNodes.filter({(startNode) => startNode != node})
    val filteredExitNodes = exitNodes.filter({(exitNode) => exitNode != node})
    val filteredNodes = nodes.filter({(otherNode) => otherNode != node})
    val filteredEdges = edges.foldLeft(Map[Node, List[Node]]()) {(acc, entry) => if (entry._1 == node) acc else acc + (entry._1 -> entry._2.filter({(succ) => succ != node}))}
    return new ControlFlowGraph(filteredEntryNodes, filteredExitNodes, filteredNodes, filteredEdges).connectNodes(getNodePredecessors(node), getNodeSuccessors(node))
  }
  
  def generateGraphvizGraph() : GraphvizExporter.Graph = {
    def nodeToString(node: Node) : String = node.toString()

    var nodeMap = this.nodes.foldLeft(Map() : Map[Node, GraphvizExporter.Node])(
        (map,node) => map + ((node, GraphvizExporter.Node(nodeToString(node)))))

    def getNodeId(node: Node, nodeMap: Map[Node,GraphvizExporter.Node]) : String = nodeMap.get(node) match {
      case Some(n) => n.id 
      case None => ""
    }

    var graphNodes = nodeMap.values.toList

    var graphEdges = this.edges.foldLeft(List() : List[GraphvizExporter.Edge]) {(list, pair) =>
        var (from, toList) = pair
        toList.foldLeft(list)((list, to) => GraphvizExporter.Edge(getNodeId(from, nodeMap), getNodeId(to, nodeMap)) :: list)}

    return new GraphvizExporter.Graph {
      def nodes = graphNodes
      def edges() = graphEdges
      def subgraphs() = List()
      def name() = "ControlFlowGraph"
    }
  }
}

object ControlFlowGraph {
  final val EMPTY = new ControlFlowGraph(List(), List(), List(), Map())
  
  final def makeSingleton(node: Node): ControlFlowGraph = {
    val nodes = node :: List()
    return new ControlFlowGraph(nodes, nodes, nodes, Map())
  }
}