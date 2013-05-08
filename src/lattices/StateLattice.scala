package tapy.lattices

import tapy.dfa._
import tapy.cfg._

object StateLattice extends ProductLattice(
  new MapLattice(ObjectLattice),
  new ProductLattice(
    StackLattice,
    new ProductLattice(
      new PowerSubSetLattice[String](),
      new PowerSubSetLattice[String]())))