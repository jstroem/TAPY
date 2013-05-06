package tapy.lattices

import tapy.dfa._

// T: The type of allocation sites
class StackLattice[L] extends ProductLattice(
  new MapLattice(new ValueLattice[L]()),
  new PowerSubSetLattice[(List[L], L, L)]()) // Execution context