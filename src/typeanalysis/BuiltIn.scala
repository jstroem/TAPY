package tapy.typeanalysis

import tapy.dfa._
import tapy.cfg._

trait BuiltInClass[T] {
	def lookupSelf(state: AnalysisLattice.Elt, node: Node, selfReg: Int) : ObjectLabelLattice.Elt = {
		val stackFrame = StackFrame = AnalysisLattice.getStackFrame(node, state)
		val selfObject = StackFrameLattice.getRegisterValue(stackFrame, selfReg)
		if (ValueLattice.elementIsOnlyObjectLabels(selfObject)) {
			return ValueLattice.getObjectLabels(selfObject)
		} else {
			throw new TypeError("Builtin Method call with a self value that wasn't only objectlabels")
		}
	}
}

trait BuiltInFunction {
	def execute(state: AnalysisLattice.Elt, args: List[Int]) : ValueLattice.Elt

	val name : String
}

object PyList(state: AnalysisLattice.Elt) extends BuiltInClass[PyList] {

	case object append extends BuiltInFunction {
		def execute(state: AnalysisLattice.Elt, node: Node, argReg: List[Int]) : ValueLattice.Elt = {

		}
	} 
}