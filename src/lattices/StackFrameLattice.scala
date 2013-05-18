package tapy.lattices

import tapy.dfa._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

object StackFrameLattice extends MapLattice[Int, ValueLattice.Elt](ValueLattice) {

  /* Getters */
  
  def getRegisterValue(el: Elt, register: Int): ValueLattice.Elt =
    get(el, register)

  /* Updaters */
    
  def updateRegisterValue(el: Elt, register: Int, value: ValueLattice.Elt, strong: Boolean = false): Elt = {
    if (strong)
      update(el, register, value)
    else
      update(el, register, ValueLattice.leastUpperBound(value, getRegisterValue(el, register)))
  }
}
