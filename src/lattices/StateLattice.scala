package tapy.lattices

import tapy.dfa._

// T: The type of allocation sites
class StateLattice[T]
extends ProductLattice(
    new MapLattice(new ObjectLattice[T]()),
    new ProductLattice(
        new StackLattice[T](),
        new ProductLattice(
            new PowerSubSetLattice[T](),
            new PowerSubSetLattice[T]())))