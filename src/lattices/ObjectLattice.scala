package tapy.lattices

import tapy.dfa._

class ObjectLattice(allocationSites: Set[Int], scopeChain: Set[Int])
extends ProductLattice(
    new MapLattice(
        new ProductLattice(
            new ValueLattice(allocationSites),
            new ProductLattice(
                AbsentLattice,
                ModifiedLattice))),
    new PowerSubSetLattice(scopeChain))