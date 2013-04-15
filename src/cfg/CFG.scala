package tapy.cfg

import tapy.export._

case class ControlFlowGraph(
    startNodes: List[Node],
    exitNodes: List[Node],
    nodes: List[Node],
    edges: Map[Node, List[Node]]) {
  
  def predecessors(node: Node) = {
    // TODO
  }
  
  def successors(node: Node) = {
    // TODO
  }
  
  def combine(o: ControlFlowGraph) = {
    // TODO
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