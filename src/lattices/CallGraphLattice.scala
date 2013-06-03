package tapy.lattices

import tapy.dfa._
import tapy.cfg._

/**
  * Any: Context sensitivity
  * Boolean (1): True = Function call, False = Constructor call
  * Boolean (2): True = Normal edge, False = Exception edge
  */
object CallGraphLattice extends PowerSubSetLattice[(Any, Node, Any, Node, Boolean, Boolean)] {

  /* Getters */
  
  def getPredecessors(el: CallGraphLattice.Elt, node: Node, predicate: (Any, Node, Any, Node, Boolean, Boolean) => Boolean = {(_,_,_,_,_,_) => true}): Set[Node] =
    if (el != null)
      el.foldLeft(Set[Node]()) {(acc, elt) =>
        val (predCtx, pred, succCtx, succ, funcCall, normalEdge) = elt
        if (succ == node && predicate(predCtx, pred, succCtx, succ, funcCall, normalEdge)) acc + pred else acc
      }
    else
      Set()
  
  def getPredecessorsExceptConstructorReturn(el: CallGraphLattice.Elt, node: Node): Set[Node] = {
    getPredecessors(el, node, {(_, pred, _, _, funcCall, _) =>
      if (funcCall)
        true
      else
        pred match {
          case pred: FunctionExitNode => false
          case pred => true
        }
    })
  }
  
  def getExceptionPredecessors(el: CallGraphLattice.Elt, node: Node): Set[Node] = {
    getPredecessors(el, node, ((_, pred, _, _, _, normal) => !normal))
  }
  
  def getFunctionCallPredecessors(el: CallGraphLattice.Elt, node: Node): Set[Node] = {
    getPredecessors(el, node, {(_, _, _, _, funcCall, _) => funcCall})
  }
  
  def getConstructorCallPredecessors(el: CallGraphLattice.Elt, node: Node): Set[Node] =
    getPredecessors(el, node, {(_, _, _, _, funcCall, _) => !funcCall})
  
  def getSuccessors(el: CallGraphLattice.Elt, node: Node, predicate: (Any, Node, Any, Node, Boolean, Boolean) => Boolean = {(_,_,_,_,_,_) => true}): Set[Node] =
    if (el != null)
      el.foldLeft(Set[Node]()) {(acc, elt) =>
        val (predCtx, pred, succCtx, succ, funcCall, normalEdge) = elt
        if (pred == node && predicate(predCtx, pred, succCtx, succ, funcCall, normalEdge)) acc + succ else acc
      }
    else
      Set()
  
  def getFunctionCallSuccessors(el: CallGraphLattice.Elt, node: Node): Set[Node] = {
    getSuccessors(el, node, {(_, _, _, _, funcCall, _) => funcCall})
  }
  
  def getConstructorCallSuccessors(el: CallGraphLattice.Elt, node: Node): Set[Node] =
    getSuccessors(el, node, {(_, _, _, _, funcCall, _) => !funcCall})
  
  def getExceptionSuccessors(el: CallGraphLattice.Elt, node: Node): Set[Node] = {
    getSuccessors(el, node, ((_, _, _, _, _, normal) => !normal))
  }
}