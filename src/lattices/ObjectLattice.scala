package tapy.lattices

import tapy.dfa._

object ObjectLattice extends ProductLattice(
  new MapLattice(
    new ProductLattice(
      ValueLattice,
      new ProductLattice(
          AbsentLattice,
          ModifiedLattice))),
  new PowerSubSetLattice[String]())