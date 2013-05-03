package tapy.lattices

import tapy.dfa._

// T: The type of allocation sites
class StackLattice[T]
extends ProductLattice(
    new MapLattice(new ValueLattice[T]()),
    new ProductLattice(
        new PowerSubSetLattice[(List[T], T, T)](), // Execution context
        new PowerSubSetLattice[T]()))