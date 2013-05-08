package tapy.lattices

import tapy.dfa._

object StackFrameLattice extends MapLattice[Int, ValueLattice.Elt](ValueLattice)

object ExecutionContextLattice extends PowerSubSetLattice[(List[String], String, String)]

object StackLattice extends ProductLattice(StackFrameLattice,ExecutionContextLattice)