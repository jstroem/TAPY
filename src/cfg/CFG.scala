package tapy.cfg

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
  
  def pretty_print() = {
    
  }
}