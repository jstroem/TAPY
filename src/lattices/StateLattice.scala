package tapy.lattices

import tapy.dfa._

// T: The type of allocation sites
class StateLattice[L]
extends ProductLattice(
    new MapLattice(new ObjectLattice[L]()),
    new ProductLattice(
        new StackLattice[L](),
        new ProductLattice(
            new PowerSubSetLattice[L](),
            new PowerSubSetLattice[L]())))