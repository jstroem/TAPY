package tapy.cfg

import tapy.export._

case class ControlFlowGraph(
    startNodes: List[Node],
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
  
  def combineGraphs(o: ControlFlowGraph): ControlFlowGraph = {
    return new ControlFlowGraph(startNodes ++ o.startNodes, exitNodes ++ o.exitNodes, nodes ++ o.nodes, edges ++ o.edges)
  }

  def connectNodes(pred: Node, succ: Node): ControlFlowGraph = {
    val newPredSuccessors = edges.get(pred) match {
      case Some(succs) => succ :: succs
      case None => succ :: List[Node]()
    }
    return new ControlFlowGraph(startNodes, exitNodes, nodes, edges + (pred -> newPredSuccessors))
  }

  def connectManyNodes(predecessors: List[Node], successors: List[Node]): ControlFlowGraph = {
    val newEdges = predecessors.foldLeft(edges) {(acc, pred) =>
      acc.get(pred) match {
        case Some(currentPredSuccessors) =>
          val newPredSuccessors = currentPredSuccessors ++ successors
          acc + (pred -> newPredSuccessors)
        case None => acc + (pred -> successors)
      }
    }
    return new ControlFlowGraph(startNodes, exitNodes, nodes, newEdges)
  }

  def removeNode(node: Node): ControlFlowGraph = {
    val filteredStartNodes = startNodes.filter({(startNode) => startNode != node})
    val filteredExitNodes = exitNodes.filter({(exitNode) => exitNode != node})
    val filteredNodes = nodes.filter({(otherNode) => otherNode != node})
    val filteredEdges = edges.foldLeft(Map[Node, List[Node]]()) {(acc, entry) => if (entry._1 == node) acc else acc + (entry._1 -> entry._2.filter({(succ) => succ != node}))}
    return new ControlFlowGraph(filteredStartNodes, filteredExitNodes, filteredNodes, filteredEdges).connectManyNodes(getNodePredecessors(node), getNodeSuccessors(node))
  }
  
  def generateGraphvizGraph() : GraphvizExporter.Graph = {
    def nodeToString(node: Node) : String = node.toString()

    var nodeMap : Map[Node,GraphvizExporter.Node] = this.nodes.foldLeft(Map() : Map[Node, GraphvizExporter.Node])(
        (map,node) => map + ((node, GraphvizExporter.Node(nodeToString(node)))))

    def getNodeId(node: Node, nodeMap: Map[Node,GraphvizExporter.Node]) : String = nodeMap.get(node) match {
      case Some(n) => n.id 
      case None => ""
    }

    var graphNodes : List[GraphvizExporter.Node] = nodeMap.values.toList

    var graphEdges : List[GraphvizExporter.Edge] = this.edges.foldLeft(List() : List[GraphvizExporter.Edge])(
      (list,pair) => {
        var (from,toList) = pair
          toList.foldLeft(list)((list,to) => GraphvizExporter.Edge(getNodeId(from,nodeMap),getNodeId(to,nodeMap)) :: list)
      }) 

    new GraphvizExporter.Graph {
      def nodes = graphNodes
      def edges() = graphEdges
      def subgraphs() = List()
      def name() = "ControlFlowGraph"
    }
  }
}