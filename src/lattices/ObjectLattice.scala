package tapy.lattices

import tapy.dfa._

object ObjectPropertyLattice extends ProductLattice(ValueLattice, new ProductLattice(AbsentLattice, ModifiedLattice)) {
  
  /* Getters */
  
  def getValue(el: ObjectPropertyLattice.Elt): ValueLattice.Elt = {
    val (value, _) = el
    value
  }
  
  /* Setters */
  
  def setValue(el: ObjectPropertyLattice.Elt, value: ValueLattice.Elt): ObjectPropertyLattice.Elt = {
    (value, el._2)
  }
}

object ObjectPropertiesLattice extends MapLattice[String, ObjectPropertyLattice.Elt](ObjectPropertyLattice) {
  
  /* Getters */
  
  def getProperty(el: Elt, property: String) = get(el, property)
  
  /* Setters */
  
  def setProperty(el: Elt, property: String, value: ObjectPropertyLattice.Elt): Elt = {
    update(el, property, value)
  }
  
  /* Updaters */
  
  def updatePropertyValue(el: Elt, property: String, value: ValueLattice.Elt): Elt = {
    val oldValue = getProperty(el, property)
    val newValue = ObjectPropertyLattice.setValue(oldValue, value)
    update(el, property, newValue)
  }
}

object ScopeChainPowerLattice extends PowerSubSetLattice[List[ObjectLabel]]()

object ObjectLattice extends ProductLattice(ObjectPropertiesLattice, ScopeChainPowerLattice) {
  
  /* Getters */
  
  def getProperty(el: Elt, property: String): ObjectPropertyLattice.Elt =
    ObjectPropertiesLattice.getProperty(getProperties(el), property)
  
  def getProperties(el: Elt): ObjectPropertiesLattice.Elt = {
    val (objectProperties, _) = el
    objectProperties
  }
  
  def getScopeChain(el: Elt): ScopeChainPowerLattice.Elt = {
    val (_, scopeChain) = el
    scopeChain
  }
  
  /* Setters */
  
  def setScopeChain(el: Elt, scopeChain: ScopeChainPowerLattice.Elt): Elt = {
    (getProperties(el), scopeChain)
  }
  
  def setProperty(el: Elt, property: String, value: ObjectPropertyLattice.Elt): Elt =
    (ObjectPropertiesLattice.setProperty(getProperties(el), property, value), getScopeChain(el))
  
  /* Updaters */
    
  
  def updatePropertyValue(el: Elt, property: String, value: ValueLattice.Elt): Elt =
    (ObjectPropertiesLattice.updatePropertyValue(getProperties(el), property, value), getScopeChain(el))
}
