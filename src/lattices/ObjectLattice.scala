package tapy.lattices

import tapy.dfa._

/*
 * Object Property Lattice 
 */
object PropertyLattice extends ProductLattice(ValueLattice, new ProductLattice(AbsentLattice, new ProductLattice(ModifiedLattice, GlobalLattice))) {
  
  /* Getters */
  
  def isModified(el: Elt): Boolean = {
    val (_, (_, (modified, _))) = el
    modified == ModifiedLattice.top
  }

  def isGlobal(el: Elt): Boolean = {
    val (_, (_, (_, global))) = el
    global == GlobalLattice.top
  }
  
  /* Getters */

  def getValue(el: Elt): ValueLattice.Elt = {
    val (value, _) = el
    value
  }

  def getAbsent(el: Elt): AbsentLattice.Elt = {
    val (value, (absent, (modified, global))) = el
    absent
  }

  def getModified(el: Elt): ModifiedLattice.Elt = {
    val (value, (absent, (modified, global))) = el
    modified
  }

  def getGlobal(el: Elt): GlobalLattice.Elt = {
    val (value, (absent, (modified, global))) = el
    global
  }
  
  /* Setters */
  
  def setValue(value: ValueLattice.Elt, el: Elt = bottom): Elt =
    (value, el._2)

  def setAbsent(absent: AbsentLattice.Elt, el: Elt = bottom): Elt = {
    val (value, (_, (modified, global))) = el
    (value, (absent, (modified, global)))
  }

  def setModified(modified: ModifiedLattice.Elt, el: Elt = bottom): Elt = {
    val (value, (absent, (_, global))) = el
    (value, (absent, (modified, global)))
  }

  def setGlobal(global: GlobalLattice.Elt, el: Elt = bottom): Elt = {
    val (value, (absent, (modified, _))) = el
    (value, (absent, (modified, global)))
  }
}

/*
 * Object Properties Lattice 
 */
object PropertiesLattice extends MapLattice[String, PropertyLattice.Elt](PropertyLattice) {
  
  /* Getters */
  
  def getProperty(el: Elt, property: String) =
    get(el, property)
  
  def getPropertyValue(el: Elt, property: String) =
    PropertyLattice.getValue(getProperty(el, property))
    
  /* Setters */
    
  def setProperty(el: Elt, property: String, value: PropertyLattice.Elt): Elt =
    update(el, property, value)
  
  /* Updaters */

  def updatePropertyValue(el: Elt, property: String, value: ValueLattice.Elt): Elt = {
    val currProperty = getProperty(el, property)
    val newProperty = PropertyLattice.setValue(value, currProperty)
    update(el, property, newProperty)
  }

  def updatePropertyAbsent(el: Elt, property: String, absent: AbsentLattice.Elt): Elt = {
    val currProperty = getProperty(el, property)
    val newProperty = PropertyLattice.setAbsent(absent, currProperty)
    update(el, property, newProperty)
  }

  def updatePropertyModified(el: Elt, property: String, modified: ModifiedLattice.Elt): Elt = {
    val currProperty = getProperty(el, property)
    val newProperty = PropertyLattice.setModified(modified, currProperty)
    update(el, property, newProperty)
  }

  def updatePropertyGlobal(el: Elt, property: String, global: GlobalLattice.Elt): Elt = {
    val currProperty = getProperty(el, property)
    val newProperty = PropertyLattice.setGlobal(global, currProperty)
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
object ObjectLattice extends ProductLattice(PropertiesLattice, ScopeChainPowerLattice) {
  
  /* Getters */
  
  def getProperty(el: Elt, property: String): PropertyLattice.Elt = {
    PropertiesLattice.getProperty(getProperties(el), property)
  }
  
  def getPropertyValue(el: Elt, property: String): ValueLattice.Elt =
    PropertiesLattice.getPropertyValue(getProperties(el), property)
  
  def getProperties(el: Elt): PropertiesLattice.Elt = {
    val (objectProperties, _) = el
    objectProperties
  }
  
  def getScopeChain(el: Elt): ScopeChainPowerLattice.Elt = {
    val (_, scopeChain) = el
    scopeChain
  }
  
  /* Setters */
  
  def setScopeChain(scopeChain: ScopeChainPowerLattice.Elt, el: Elt = bottom): Elt =
    (getProperties(el), scopeChain)
  
  def setProperty(property: String, value: PropertyLattice.Elt, el: Elt = bottom): Elt =
    (PropertiesLattice.setProperty(getProperties(el), property, value), getScopeChain(el))

  /* Updaters */
  
  def updatePropertyValue(property: String, value: ValueLattice.Elt, el: Elt = bottom): Elt =
    (PropertiesLattice.updatePropertyValue(getProperties(el), property, value), getScopeChain(el))
  
  def updatePropertyValues(pairs: Set[(String, ValueLattice.Elt)], el: Elt = bottom): Elt =
    pairs.foldLeft(el) {(acc, pair) =>
      val (property, value) = pair
      (PropertiesLattice.updatePropertyValue(getProperties(acc), property, value), getScopeChain(acc))
    }

  def updatePropertyAbsent(el: Elt, property: String, absent: AbsentLattice.Elt): Elt =
    (PropertiesLattice.updatePropertyAbsent(getProperties(el), property, absent), getScopeChain(el))

  def updatePropertyModified(el: Elt, property: String, modified: ModifiedLattice.Elt): Elt =
    (PropertiesLattice.updatePropertyModified(getProperties(el), property, modified), getScopeChain(el))

  def updatePropertyGlobal(el: Elt, property: String, global: GlobalLattice.Elt): Elt =
    (PropertiesLattice.updatePropertyGlobal(getProperties(el), property, global), getScopeChain(el))
}
