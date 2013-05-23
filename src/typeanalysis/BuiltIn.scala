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
  
  val objectLabel = BuiltInClassObjectLabel("object")
  val objectElt = ObjectLattice.bottom

  val objectValue = ValueLattice.setObjectLabels(Set(objectLabel))
  val noneValue = ValueLattice.setNone(NoneLattice.top)
  val falseValue = ValueLattice.setBoolean(false)
  val trueValue = ValueLattice.setBoolean(true)
  
  /* Built in objects */
  
  def objectOverwritten(node: Node, solution: Elt): Boolean = {
    Utils.findPropertyValueInScope(node, "object", solution) != BuiltIn.objectValue
  }

  trait BuiltInClass[T] {
	// 	def lookupSelf(state: AnalysisLattice.Elt, node: Node, selfReg: Int) : ObjectLabelLattice.Elt = {
	// 		val stackFrame = StackFrame = AnalysisLattice.getStackFrame(node, state)
	// 		val selfObject = StackFrameLattice.getRegisterValue(stackFrame, selfReg)
	// 		if (ValueLattice.elementIsOnlyObjectLabels(selfObject)) {
	// 			return ValueLattice.getObjectLabels(selfObject)
	// 		} else {
	// 			throw new TypeError("Builtin Method call with a self value that wasn't only objectlabels")
	// 		}
	// 	}
	// }

	// trait BuiltInFunction {
	// 	def execute(state: AnalysisLattice.Elt, args: List[Int]) : ValueLattice.Elt

	// 	val name : String
	// }

	// object PyList(state: AnalysisLattice.Elt) extends BuiltInClass[PyList] {

	// 	case object append extends BuiltInFunction {
	// 		def execute(state: AnalysisLattice.Elt, node: Node, argReg: List[Int]) : ValueLattice.Elt = {

	// 		}
	// 	} 
	// }
}