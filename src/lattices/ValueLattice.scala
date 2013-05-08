package tapy.lattices

import tapy.dfa._

object AllocationSiteLattice extends PowerSubSetLattice[String]

object ValueLattice
extends ProductLattice(
  UndefinedLattice,
  new ProductLattice(
    NoneLattice,
    new ProductLattice(
      BooleanLattice,
      new ProductLattice(
        IntegerLattice,
        new ProductLattice(
          FloatLattice,
          new ProductLattice(
            LongLattice,
            new ProductLattice(
              ComplexLattice,
              new ProductLattice(
                StringLattice,
                AllocationSiteLattice)))))))) {


    def unpackElement(el: ValueLattice.Elt) : (UndefinedLattice.Elt,NoneLattice.Elt,BooleanLattice.Elt,IntegerLattice.Elt,FloatLattice.Elt,LongLattice.Elt,ComplexLattice.Elt,StringLattice.Elt, AllocationSiteLattice.Elt) = {
      val (undefined,(none,(boolean,(integer,(float,(long,(complex,(string,allocationSet)))))))) = el
      return (undefined,none,boolean,integer,float,long,complex,string,allocationSet)
    }

    def packElement(undefined : UndefinedLattice.Elt = UndefinedLattice.bottom, 
                    none : NoneLattice.Elt = NoneLattice.bottom, 
                    boolean : BooleanLattice.Elt = BooleanLattice.bottom,
                    integer : IntegerLattice.Elt = IntegerLattice.bottom,
                    float : FloatLattice.Elt = FloatLattice.bottom, 
                    long : LongLattice.Elt = LongLattice.bottom, 
                    complex : ComplexLattice.Elt = ComplexLattice.bottom,
                    string : StringLattice.Elt = StringLattice.bottom, 
                    allocationSet : AllocationSiteLattice.Elt = AllocationSiteLattice.bottom) : ValueLattice.Elt = {
      return (undefined,(none,(boolean,(integer,(float,(long,(complex,(string,allocationSet))))))))
    }
  }