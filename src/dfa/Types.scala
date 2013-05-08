package tapy.dfa

import tapy.cfg.Node

object MonotoneFrameworkTypes {
  type Constraint[T] = T => T
  type ConstraintMap[T] = Map[Node, Constraint[T]]
}
