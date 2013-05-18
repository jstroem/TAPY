package tapy.lattices

import java.util.UUID
import tapy.export._
import tapy.dfa._
import tapy.cfg._
import java.io._

object HeapLattice extends MapLattice[ObjectLabel, ObjectLattice.Elt](ObjectLattice) {
  
  /* Getters */
  
  def getObject(el: Elt, label: ObjectLabel): ObjectLattice.Elt =
    get(el, label)
  
  def getObject(el: MapElt, label: ObjectLabel): ObjectLattice.Elt =
    get(Concrete(el), label)
  
  def getObjects(el: Elt, labels: Set[ObjectLabel]): Set[ObjectLattice.Elt] =
    labels.foldLeft(Set[ObjectLattice.Elt]()) {(acc, label) => acc + getObject(el, label)}
  
  /* Updaters */
  
  def updateHeap(el: Elt, label: ObjectLabel, obj: ObjectLattice.Elt): Elt =
    update(el, label, obj)

  def exportToFile(el: Elt, fileName: String) = {
    GraphvizExporter.export(generateGraphvizGraph(el), new PrintStream(fileName + ".heap.dot"))
    Runtime.getRuntime().exec("dot -Tgif -o " + fileName + ".heap.gif " + fileName + ".heap.dot")
  }

  /* Dot export */
  def generateGraphvizGraph(el: Elt) : GraphvizExporter.Graph = {
    val emptyPair : (List[GraphvizExporter.Node],List[GraphvizExporter.Edge]) = (List(),List())
    
    val (nodeList,edgeList) = el match {
      case Concrete(map: Map[ObjectLabel, ObjectLattice.Elt]) => {
        val labelIds = map.map({ case (k,v) => (k, UUID.randomUUID().toString())})
        
        map.foldLeft(emptyPair)((acc,pair) => {
          val (accNodes,accEdges) = acc
          val (objectLabel,objectElement) = pair
  
          val objectProperties = ObjectLattice.getProperties(objectElement)
          val nodeName = GraphvizExporter.escape(objectLabel.toString())
          val (nodeLabel,edges) = objectProperties match {
            case PropertiesLattice.Concrete(objectMap: Map[String, PropertyLattice.Elt]) => {
              objectMap.foldLeft((nodeName,List()) : (String,List[GraphvizExporter.Edge]))((accPair,pair) => {
                var (nodeName,edges) = accPair
                val (propertyName,objectProperty) = pair
                val valueElement = PropertyLattice.getValue(objectProperty)
  
                val escapedPropertyName = GraphvizExporter.escape(propertyName)
  
                nodeName += ("|{%s|<%s> %s}".format(escapedPropertyName, escapedPropertyName, GraphvizExporter.escape(ValueLattice.toString(valueElement))))
  
                val objectLabels = ValueLattice.getObjectLabels(valueElement)
                edges = ValueLattice.getObjectLabels(valueElement).foldLeft(edges)((edges, toObjectLabel) => {
                  GraphvizExporter.Edge(GraphvizExporter.MultiNodeId(List(labelIds.getOrElse(objectLabel, ""),escapedPropertyName)),GraphvizExporter.SingleNodeId(labelIds.getOrElse(toObjectLabel, ""))) :: edges
                })
                (nodeName,edges)
              })
            }
            case _ => (nodeName,List())
          }
          (GraphvizExporter.Node("{"+nodeLabel+"}",GraphvizExporter.SingleNodeId(labelIds.getOrElse(objectLabel, ""))) :: accNodes, edges ::: accEdges)
        })
      }
      case _ => emptyPair
    }

    return new GraphvizExporter.Graph {
      def nodes() = nodeList
      def edges() = edgeList
      def subgraphs() = List()
      def name() = "HeapLattice"
    }
  }
}
