package tapy.typeanalysis

import tapy.constants.StackConstants
import java.lang.ArithmeticException
import org.python.antlr.ast.arguments
import org.python.antlr.ast.Name
import tapy.dfa._
import tapy.dfa.MonotoneFrameworkTypes._
import tapy.cfg._
import tapy.lattices._
import tapy.exceptions._
import tapy.constants
import scala.collection.JavaConversions._

object BuiltIn {
  type Elt = AnalysisLattice.Elt

  /** Traits for builtin classes and functions **/
  trait BuiltInClass {
    val label : BuiltInClassObjectLabel
    val valueReference : ValueLattice.Elt
    val objectInstance : ObjectLattice.Elt

    def lookupSelf(state: AnalysisLattice.Elt, node: Node, selfReg: Int) : ObjectLabelLattice.Elt = {
      val stackFrame = AnalysisLattice.getStackFrame(node, state)
      val selfObject = StackFrameLattice.getRegisterValue(stackFrame, selfReg)
      if (ValueLattice.elementIsOnlyObjectLabels(selfObject)) {
        return ValueLattice.getObjectLabels(selfObject)
      } else {
        throw new TypeError("Builtin Method call with a self value that wasn't only objectlabels")
      }
    }

    def getHeapSet() : Set[(ObjectLabel, ObjectLattice.Elt)]
    def createInstanceObjectLabel(classLabel : BuiltInClassObjectLabel, callNode : Node) : ObjectLabel
  }

  trait BuiltInFunction {
    def execute(state: AnalysisLattice.Elt, args: List[ValueLattice.Elt]) : AnalysisLattice.Elt
    var name : String
  }

  /* Utility functions */
  
  def objectOverwritten(node: Node, solution: Elt): Boolean = {
    Utils.findPropertyValueInScope(node, "object", solution) != PyObject.valueReference
  }
  
  /**
   * Values
   */
  val noneValue = ValueLattice.setNone(NoneLattice.top)
  val falseValue = ValueLattice.setBoolean(false)
  val trueValue = ValueLattice.setBoolean(true)
  
  /**
   * Functions
   */
  
  val floatFunctionLabel = BuiltInFunctionObjectLabel("float", new BuiltInFunction(){
    def execute(state: AnalysisLattice.Elt, args: List[ValueLattice.Elt]) : AnalysisLattice.Elt = {
      state
    }
    var name = "float"
  });
  val floatFunctionValue = ValueLattice.setObjectLabels(Set(floatFunctionLabel))

  /** Classes **/
  object PyObject extends BuiltInClass {
    val label = BuiltInClassObjectLabel("object", this)
    val valueReference = ValueLattice.setObjectLabels(Set(label))

    var initFunctionLabel = BuiltInFunctionObjectLabel("__init__", new BuiltInFunction() {
      var name = "object __init__"
      def execute(state: AnalysisLattice.Elt, args: List[ValueLattice.Elt]) : AnalysisLattice.Elt = {
        state
      }
    })


    val objectInstance = ObjectLattice.updatePropertyValues(Set(("__init__", ValueLattice.setObjectLabels(Set(initFunctionLabel)))))

    def getHeapSet() = {
      Set[(ObjectLabel, ObjectLattice.Elt)]((label, objectInstance),
                                            (initFunctionLabel, ObjectLattice.bottom))
    }

    def createInstanceObjectLabel(classLabel: BuiltInClassObjectLabel, callNode : Node) : ObjectLabel = {
      BuiltInInstanceObjectLabel(classLabel)
    }
  }

/** Built in implementation of List. Mockup is used instead
	object PyList extends BuiltInClass {
    val label = BuiltInClassObjectLabel("list", this)
    val valueReference = ValueLattice.setObjectLabels(Set(label))

    var appendFunctionLabel = BuiltInFunctionObjectLabel("append", new BuiltInFunction() {
      def execute(state: AnalysisLattice.Elt, args: List[ValueLattice.Elt]) : AnalysisLattice.Elt = {
        println("list append called")
        state
      }
      var name = "list append"
    })

    var initFunctionLabel = BuiltInFunctionObjectLabel("__init__", new BuiltInFunction() {
      var name = "list __init__"
      def execute(state: AnalysisLattice.Elt, args: List[ValueLattice.Elt]) : AnalysisLattice.Elt = {
        state
      }
    })

    val objectInstance = ObjectLattice.updatePropertyValues(Set(("append", ValueLattice.setObjectLabels(Set(appendFunctionLabel))),
                                                                ("__init__", ValueLattice.setObjectLabels(Set(initFunctionLabel)))))

    def getHeapSet() = {
      Set[(ObjectLabel, ObjectLattice.Elt)]((label, objectInstance),
                                            (appendFunctionLabel, ObjectLattice.bottom),
                                            (initFunctionLabel, ObjectLattice.bottom))
    }

    def createInstanceObjectLabel(classLabel: BuiltInClassObjectLabel, callNode : Node) : ObjectLabel = {
      BuiltInInstanceObjectLabel(classLabel)
    }
	}**/
}