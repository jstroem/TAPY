package tapy.lattices

import tapy.dfa._

object ObjectPropertyLattice extends ProductLattice(ValueLattice, new ProductLattice( AbsentLattice, ModifiedLattice)) {
  
  /* Updaters */
  
  def updatePropertyValue(el: ObjectPropertyLattice.Elt, property: String, value: ValueLattice.Elt): ObjectPropertyLattice.Elt = {
    (value, el._2)
  }
}

object ObjectPropertiesLattice extends MapLattice[String, ObjectPropertyLattice.Elt](ObjectPropertyLattice) {
  
  /* Updaters */
  
  def updatePropertyValue(el: ObjectPropertiesLattice.Elt, property: String, value: ValueLattice.Elt): ObjectPropertiesLattice.Elt = {
    val oldValue = el.getOrElse(property, ObjectPropertyLattice.bottom)
    val newValue = ObjectPropertyLattice.updatePropertyValue(oldValue, property, value)
    el + (property -> newValue)
  }
}

object ScopeChainLattice extends PowerSubSetLattice[String]()

object ObjectLattice extends ProductLattice(ObjectPropertiesLattice, ScopeChainLattice) {
  
  /* Getters */
  
  def getObjectProperties(el: ObjectLattice.Elt): ObjectPropertiesLattice.Elt = {
    val (objectProperties, _) = el
    objectProperties
  }
  
  def getScopeChain(el: ObjectLattice.Elt): ScopeChainLattice.Elt = {
    val (_, scopeChain) = el
    scopeChain
  }
  
  /* Updaters */
  
  def updatePropertyValue(el: ObjectLattice.Elt, property: String, value: ValueLattice.Elt): ObjectLattice.Elt =
    (ObjectPropertiesLattice.updatePropertyValue(getObjectProperties(el), property, value), getScopeChain(el))
}