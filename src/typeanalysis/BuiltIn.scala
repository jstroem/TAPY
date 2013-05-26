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
  
  /**
   * Objects
   */
  val objectLabel = BuiltInClassObjectLabel("object")
  val objectValue = ValueLattice.setObjectLabels(Set(objectLabel))
  
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
    def execute(state: AnalysisLattice.Elt, args: List[Int]) : AnalysisLattice.Elt = {
      state
    }
    var name = "float"
  });
  val floatFunctionValue = ValueLattice.setObjectLabels(Set(floatFunctionLabel))


  /* Utility functions */
  
  def objectOverwritten(node: Node, solution: Elt): Boolean = {
    Utils.findPropertyValueInScope(node, "object", solution) != BuiltIn.objectValue
  }

 trait BuiltInClass {
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
	}

	trait BuiltInFunction {
		def execute(state: AnalysisLattice.Elt, args: List[Int]) : AnalysisLattice.Elt
		var name : String
	}

	object PyList extends BuiltInClass {
    var label = BuiltInClassObjectLabel("list")
    var objectInstance = ObjectLattice.bottom

    var appendFunctionLabel = BuiltInFunctionObjectLabel("append", new BuiltInFunction() {
      def execute(state: AnalysisLattice.Elt, args: List[Int]) : AnalysisLattice.Elt = state
      var name = "list append"
    })

    objectInstance = ObjectLattice.updatePropertyValue("append", ValueLattice.setObjectLabels(Set(appendFunctionLabel)), objectInstance)

    def getHeapSet() = {
      Set[(ObjectLabel, ObjectLattice.Elt)]((label, objectInstance),
                                            (appendFunctionLabel, ObjectLattice.bottom))
    }
	}
}