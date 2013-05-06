package tapy.lattices

import tapy.dfa._

class ObjectLattice[L] extends ProductLattice(
  new MapLattice(
    new ProductLattice(
      new ValueLattice[L](),
      new ProductLattice(
          AbsentLattice,
          ModifiedLattice))),
  new PowerSubSetLattice[L]())