package tapy.lattices

import tapy.dfa._
import tapy.cfg._

// Any: Context sensitivity, Boolean: True = Function call, False = Constructor call
object CallGraphLattice extends PowerSubSetLattice[(Any, Node, Any, Node, Boolean)] {

  /* Getters */
  
  def getPredecessors(el: CallGraphLattice.Elt, node: Node, predicate: (Any, Node, Any, Node, Boolean) => Boolean = {(_,_,_,_,_) => true}): Set[Node] =
    if (el != null)
      el.foldLeft(Set[Node]()) {(acc, elt) =>
        val (predCtx, pred, succCtx, succ, funcCall) = elt
        if (succ == node && predicate(predCtx, pred, succCtx, succ, funcCall)) acc + pred else acc
      }
    else
      Set()
  
  def getPredecessorsExceptConstructorReturn(el: CallGraphLattice.Elt, node: Node): Set[Node] = {
    getPredecessors(el, node, {(_, pred, _, _, funcCall) =>
      if (funcCall)
        true
      else
        pred match {
          case pred: ExitNode => false
          case pred => true
        }
    })
  }
  
  def getFunctionCallPredecessors(el: CallGraphLattice.Elt, node: Node): Set[Node] = {
    getPredecessors(el, node, {(_, _, _, _, funcCall) => funcCall})
  }
  
  def getConstructorCallPredecessors(el: CallGraphLattice.Elt, node: Node): Set[Node] =
    getPredecessors(el, node, {(_, _, _, _, funcCall) => !funcCall})
  
  def getSuccessors(el: CallGraphLattice.Elt, node: Node, predicate: (Any, Node, Any, Node, Boolean) => Boolean = {(_,_,_,_,_) => true}): Set[Node] =
    if (el != null)
      el.foldLeft(Set[Node]()) {(acc, elt) =>
        val (predCtx, pred, succCtx, succ, funcCall) = elt
        if (pred == node && predicate(predCtx, pred, succCtx, succ, funcCall)) acc + succ else acc
      }
    else
      Set()
  
  def getFunctionCallSuccessors(el: CallGraphLattice.Elt, node: Node): Set[Node] = {
    getSuccessors(el, node, {(_, _, _, _, funcCall) => funcCall})
  }
  
  def getConstructorCallSuccessors(el: CallGraphLattice.Elt, node: Node): Set[Node] =
    getSuccessors(el, node, {(_, _, _, _, funcCall) => !funcCall})
}