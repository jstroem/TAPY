package tapy.lattices

import tapy.dfa._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

object StackFrameLattice extends MapLattice[Int, ValueLattice.Elt](ValueLattice) {

  /* Getters */
  
  def getRegisterValue(el: StackFrameLattice.Elt, register: Int): ValueLattice.Elt =
    get(el, register)

  /* Updaters */
    
  def updateRegisterValue(el: StackFrameLattice.Elt, register: Int, value: ValueLattice.Elt): StackFrameLattice.Elt =
    update(el, register, value)

}
