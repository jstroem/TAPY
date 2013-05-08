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


    def lubElement(t: ValueLattice.Elt, el : String) : ValueLattice.Elt = lubElement(t, StringLattice.Concrete(el))

    def lubElement(t: ValueLattice.Elt, el : StringLattice.Elt) : ValueLattice.Elt = {
      val (undefined,none,boolean,integer,float,long,complex,string,allocationSet) = ValueLattice.unpackElement(t)
      putElement(t, StringLattice.leastUpperBound(string, el))
    }

    def putElement(t: ValueLattice.Elt, el : String) : ValueLattice.Elt = putElement(t, StringLattice.Concrete(el))

    def putElement(t: ValueLattice.Elt, el : StringLattice.Elt) : ValueLattice.Elt = {
      val (undefined,none,boolean,integer,float,long,complex,string,allocationSet) = ValueLattice.unpackElement(t) 
      ValueLattice.packElement(undefined,none,boolean,integer,float,long,complex,el,allocationSet)
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