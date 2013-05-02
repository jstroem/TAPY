package tapy.dfa

import tapy.cfg.Node

object MonotoneFrameworkTypes {
  type Solution[T] = Map[Node, T]
  type Constraint[T] = Solution[T] => T
  type ConstraintMap[T] = Map[Node, Constraint[T]]
}
