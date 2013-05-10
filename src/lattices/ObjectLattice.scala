package tapy.lattices

import tapy.dfa._

object ObjectPropertyLattice extends ProductLattice(ValueLattice, new ProductLattice(AbsentLattice, ModifiedLattice)) {
  
  /* Getters */
  
  def getValue(el: ObjectPropertyLattice.Elt): ValueLattice.Elt = {
    val (value, _) = el
    value
  }
  
  /* Updaters */
  
  def updatePropertyValue(el: ObjectPropertyLattice.Elt, property: String, value: ValueLattice.Elt): ObjectPropertyLattice.Elt = {
    (value, el._2)
  }
}

object ObjectPropertiesLattice extends MapLattice[String, ObjectPropertyLattice.Elt](ObjectPropertyLattice) {
  
  /* Getters */
  
  def getProperty(el: ObjectPropertiesLattice.Elt, property: String) = get(el, property)
  
  /* Updaters */
  
  def updatePropertyValue(el: ObjectPropertiesLattice.Elt, property: String, value: ValueLattice.Elt): ObjectPropertiesLattice.Elt = {
    val oldValue = getProperty(el, property)
    val newValue = ObjectPropertyLattice.updatePropertyValue(oldValue, property, value)
    update(el, property, newValue)
  }
}

object ScopeChainPowerLattice extends PowerSubSetLattice[List[ObjectLabel]]()

object ObjectLattice extends ProductLattice(ObjectPropertiesLattice, ScopeChainPowerLattice) {
  
  /* Getters */
  
  def getObjectProperty(el: Elt, property: String): ObjectPropertyLattice.Elt =
    ObjectPropertiesLattice.getProperty(getObjectProperties(el), property)
  
  def getObjectProperties(el: Elt): ObjectPropertiesLattice.Elt = {
    val (objectProperties, _) = el
    objectProperties
  }
  
  def getScopeChain(el: Elt): ScopeChainPowerLattice.Elt = {
    val (_, scopeChain) = el
    scopeChain
  }
  
  /* Setters */
  
  def setScopeChain(el: Elt, scopeChain: ScopeChainPowerLattice.Elt): Elt = {
    (getObjectProperties(el), scopeChain)
  }
  
  /* Updaters */
  
  def updatePropertyValue(el: Elt, property: String, value: ValueLattice.Elt): Elt =
    (ObjectPropertiesLattice.updatePropertyValue(getObjectProperties(el), property, value), getScopeChain(el))
}
