package tapy.lattices

import tapy.dfa._

/*
 * Object Property Lattice 
 */
object ObjectPropertyLattice extends ProductLattice(ValueLattice, new ProductLattice(AbsentLattice, new ProductLattice(ModifiedLattice, GlobalLattice))) {
  
  /* Getters */  
  def getValue(el: Elt): ValueLattice.Elt = {
    val (value, _) = el
    value
  }

  def getAbsent(e: Elt): AbsentLattice.Elt = {
    val (value, (absent, (modified, global))) = e
    absent
  }

  def getModified(e: Elt): ModifiedLattice.Elt = {
    val (value, (absent, (modified, global))) = e
    modified
  }

  def getGlobal(e: Elt): GlobalLattice.Elt = {
    val (value, (absent, (modified, global))) = e
    global
  }
  
  /* Setters */
  def setValue(el: Elt, value: ValueLattice.Elt): ObjectPropertyLattice.Elt = {
    (value, el._2)
  }

  def setAbsent(e: Elt, replacement: AbsentLattice.Elt): Elt = {
    val (value, (absent, (modified, global))) = e
    (value, (replacement, (modified, global)))
  }

  def setModified(e: Elt, replacement: ModifiedLattice.Elt): Elt = {
    val (value, (absent, (modified, global))) = e
    (value, (absent, (replacement, global)))
  }

  def setGlobal(e: Elt, replacement: GlobalLattice.Elt): Elt = {
    val (value, (absent, (modified, global))) = e
    (value, (absent, (modified, replacement)))
  }
}

/*
 * Object Properties Lattice 
 */
object ObjectPropertiesLattice extends MapLattice[String, ObjectPropertyLattice.Elt](ObjectPropertyLattice) {
  
  /* Getters */
  def getProperty(el: Elt, property: String) = get(el, property)
  
  /* Setters */
  def setProperty(el: Elt, property: String, value: ObjectPropertyLattice.Elt): Elt = {
    update(el, property, value)
  }
  
  /* Updaters */
  def updatePropertyValue(el: Elt, property: String, value: ValueLattice.Elt): Elt = {
    val currProperty = getProperty(el, property)
    val newProperty = ObjectPropertyLattice.setValue(currProperty, value)
    update(el, property, newProperty)
  }

  def updatePropertyAbsent(el: Elt, property: String, absent: AbsentLattice.Elt): Elt = {
    val currProperty = getProperty(el, property)
    val newProperty = ObjectPropertyLattice.setAbsent(currProperty, absent)
    update(el, property, newProperty)
  }

  def updatePropertyModified(el: Elt, property: String, modified: ModifiedLattice.Elt): Elt = {
    val currProperty = getProperty(el, property)
    val newProperty = ObjectPropertyLattice.setModified(currProperty, modified)
    update(el, property, newProperty)
  }

  def updatePropertyGlobal(el: Elt, property: String, global: GlobalLattice.Elt): Elt = {
    val currProperty = getProperty(el, property)
    val newProperty = ObjectPropertyLattice.setGlobal(currProperty, global)
    update(el, property, newProperty)
  }
}

/*
 * Scope Chain Power Lattice
 */
object ScopeChainPowerLattice extends PowerSubSetLattice[List[ObjectLabel]]()

/*
 * Object Lattice 
 */
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
