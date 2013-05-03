package tapy.lattices

import tapy.dfa._

class ObjectLattice[T]
extends ProductLattice(
    new MapLattice(
        new ProductLattice(
            new ValueLattice(),
            new ProductLattice(
                AbsentLattice,
                ModifiedLattice))),
    new PowerSubSetLattice[T]())