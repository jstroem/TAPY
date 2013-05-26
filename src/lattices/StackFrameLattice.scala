package tapy.lattices

import tapy.dfa._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

object StackFrameLattice extends MapLattice[Int, ValueLattice.Elt](ValueLattice) {

  /* Getters */
  
  def getRegisterValue(el: Elt, reg: Int): ValueLattice.Elt =
    get(el, reg)
  
  def getRegisterValues(el: Elt, regs: Set[Int]): ValueLattice.Elt =
    regs.foldLeft(ValueLattice.bottom) {(acc, reg) =>
      ValueLattice.leastUpperBound(acc, get(el, reg))
    }

  /* Updaters */
    
  def updateRegisterValue(el: Elt, register: Int, value: ValueLattice.Elt, strong: Boolean = false): Elt = {
    if (strong)
      update(el, register, value)
    else
      update(el, register, ValueLattice.leastUpperBound(value, getRegisterValue(el, register)))
  }
}
