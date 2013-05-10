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
    val oldValue = el.getOrElse(property, ObjectPropertyLattice.bottom)
    val newValue = ObjectPropertyLattice.updatePropertyValue(oldValue, property, value)
    el + (property -> newValue)
  }
}

object ScopeChainPowerLattice extends PowerSubSetLattice[List[ObjectLabel]]()

object ObjectLattice extends ProductLattice(ObjectPropertiesLattice, ScopeChainPowerLattice) {
  
  /* Getters */
  
  def getObjectProperty(el: ObjectLattice.Elt, property: String): ObjectPropertyLattice.Elt =
    ObjectPropertiesLattice.getProperty(getObjectProperties(el), property)
  
  def getObjectProperties(el: ObjectLattice.Elt): ObjectPropertiesLattice.Elt = {
    val (objectProperties, _) = el
    objectProperties
  }
  
  def getScopeChain(el: ObjectLattice.Elt): ScopeChainPowerLattice.Elt = {
    val (_, scopeChain) = el
    scopeChain
  }
  
  /* Updaters */
  
  def updatePropertyValue(el: ObjectLattice.Elt, property: String, value: ValueLattice.Elt): ObjectLattice.Elt =
    (ObjectPropertiesLattice.updatePropertyValue(getObjectProperties(el), property, value), getScopeChain(el))
}