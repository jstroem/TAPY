package tapy.lattices

import tapy.dfa._

object StackLattice extends ProductLattice(
  new MapLattice(ValueLattice),
  new PowerSubSetLattice[(List[String], String, String)]()) // Execution context