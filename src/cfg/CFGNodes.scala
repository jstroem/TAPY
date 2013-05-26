package tapy.cfg

import org.python.core._
import java.util.UUID
import tapy.constants
import org.python.antlr.ast.cmpopType
import org.python.antlr.ast.operatorType
import org.python.antlr.ast.unaryopType
import tapy.lattices._

abstract class Node(id: UUID) {
  protected def reg(r: Int) = s"<$r>"
  
  def getState(el: AnalysisLattice.Elt): StateLattice.Elt = AnalysisLattice.getState(this, el)
  def setState(el: AnalysisLattice.Elt, state: StateLattice.Elt = StateLattice.bottom): AnalysisLattice.Elt = AnalysisLattice.setState(el, this, state)
  
  def getHeap(el: AnalysisLattice.Elt): Map[ObjectLabel, ObjectLattice.Elt] = AnalysisLattice.getHeap(this, el) match {
    case HeapLattice.Top() => null
    case HeapLattice.Concrete(map) => map
    case _ => throw new InternalError()
  }
  
  def getObject(el: AnalysisLattice.Elt, label: ObjectLabel): ObjectLattice.Elt = AnalysisLattice.getHeapObject(this, label, el)
  
  def getProperty(el: AnalysisLattice.Elt, label: ObjectLabel, property: String): PropertyLattice.Elt = ObjectLattice.getProperty(getObject(el, label), property)
  
  def getPropertyValue(el: AnalysisLattice.Elt, label: ObjectLabel, property: String): ValueLattice.Elt = PropertyLattice.getValue(getProperty(el, label, property))
  
  def getStack(el: AnalysisLattice.Elt): StackLattice.Elt = AnalysisLattice.getStack(this, el)
  
  def getStackFrame(el: AnalysisLattice.Elt): StackFrameLattice.Elt = AnalysisLattice.getStackFrame(this, el)
  
  def getRegisterValues(el: AnalysisLattice.Elt, regs: Set[Int]): ValueLattice.Elt = StackFrameLattice.getRegisterValues(getStackFrame(el), regs)
  
  def getRegisterValue(el: AnalysisLattice.Elt, reg: Int): ValueLattice.Elt = StackFrameLattice.getRegisterValue(getStackFrame(el), reg)
  def setRegisterValue(el: AnalysisLattice.Elt, reg: Int, value: ValueLattice.Elt = ValueLattice.bottom, strong: Boolean = false): AnalysisLattice.Elt = updateStackFrame(el, reg, value, strong)
  
  def getExecutionContexts(el: AnalysisLattice.Elt): ExecutionContextLattice.Elt = StackLattice.getExecutionContext(getStack(el))
  
  def getVariableObjects(el: AnalysisLattice.Elt): Set[ObjectLabel] = StateLattice.getVariableObjects(getState(el))
  
  def updateHeap(el: AnalysisLattice.Elt, label: ObjectLabel, obj: ObjectLattice.Elt): AnalysisLattice.Elt = AnalysisLattice.updateHeap(el, this, label, obj)
  def updateHeap(el: AnalysisLattice.Elt, pairs: Set[(ObjectLabel, ObjectLattice.Elt)]): AnalysisLattice.Elt = AnalysisLattice.updateHeap(el, this, pairs)
  
  def updateStackFrame(el: AnalysisLattice.Elt, register: Int, value: ValueLattice.Elt, strong: Boolean = false): AnalysisLattice.Elt = AnalysisLattice.updateStackFrame(el, this, register, value, strong)
  def updateStackFrames(el: AnalysisLattice.Elt, pairs: Set[(Int, ValueLattice.Elt)], strong: Boolean = false): AnalysisLattice.Elt = AnalysisLattice.updateStackFrames(el, this, pairs, strong)
}

// Class and function declaration
case class FunctionDeclNode(entry: FunctionEntryNode, exit: ExitNode, exceptionalExitNode: ExceptionalExitNode, defaultArgRegs : List[Int], id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"FunctionDecl(${entry.funcDef.getInternalName()})"
}
case class ClassDeclNode(entry: ClassEntryNode, exit: ExitNode, bases: List[String], id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"ClassDecl(${entry.classDef.getInternalName()})"
}

// Class and function entries/exits
case class ClassEntryNode(note: String, bases: List[String], classDef: org.python.antlr.ast.ClassDef, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"ClassEntryNode($note)"
}
case class FunctionEntryNode(note: String, funcDef: org.python.antlr.ast.FunctionDef, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"FunctionEntryNode($note)"
}
case class ExitNode(note: String, entryNode: Node, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"ExitNode($note)"
}

case class ModuleEntryNode(name: String, moduleDef: org.python.antlr.ast.Module, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"ModuleEntryNode($name)"
}

// Write variable; variable = value
// Write property into base (object/class); base.property = value
// Write property into dictionary: base[property] = value
case class WriteVariableNode(variable: String, valueReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"$variable = ${reg(valueReg)}\t(WriteVariableNode)"
}
case class WriteRegisterNode(resultReg: Int, valueReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = ${reg(valueReg)}\t(WriteRegisterNode)"
}
case class WritePropertyNode(baseReg: Int, property: String, valueReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(baseReg)}.$property = ${reg(valueReg)}\t(WritePropertyNode)"
}
case class WriteIndexableNode(baseReg: Int, propertyReg: Int, valueReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(baseReg)}[${reg(propertyReg)}] = ${reg(valueReg)}\t(WriteIndexableNode)"
}

//Constant expressions.
case class ConstantBooleanNode(resultReg: Int, bool: Boolean, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = $bool\t(ConstantBooleanNode)"
}
case class ConstantIntNode(resultReg: Int, int: PyInteger, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = $int\t(ConstantIntNode)"
}
case class ConstantFloatNode(resultReg: Int, float: PyFloat, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = $float\t(ConstantFloatNode)"
}
case class ConstantLongNode(resultReg: Int, long: PyLong, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = $long\t(ConstantLongNode)"
}
case class ConstantComplexNode(resultReg: Int, complex: PyComplex, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = $complex\t(ConstantComplexNode)"
}
case class ConstantStringNode(resultReg: Int, string : String, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = '$string'\t(ConstantStringNode)"
}
case class ConstantNoneNode(resultReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = None\t(ConstantNoneNode)"
}

// Read variable; result = variable
// Read property from base (object/class); result =  base.property
// Read property from dictionary: result = base[property]
case class ReadVariableNode(variable: String, resultReg: Int, builtin : Boolean = false, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = {
    if (builtin) s"${reg(resultReg)} = $variable (builtin) \t(ReadVariableNode)"
    else s"${reg(resultReg)} = $variable\t(ReadVariableNode)"
  }
}
case class ReadPropertyNode(baseReg: Int, property: String, resultReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = ${reg(baseReg)}.$property\t(ReadPropertyNode)"
}
case class ReadIndexableNode(baseReg: Int, propertyReg: Int, resultReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = ${reg(baseReg)}[${reg(propertyReg)}]\t(ReadIndexableNode)"
}

case class HasAttributeNode(baseReg: Int, property: String, resultReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = hasattr(${reg(baseReg)}.$property)\t(HasAttributeNode)"
}

// Del
case class DelVariableNode(variable: String, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"del $variable\t(DelVariableNode)"
}
case class DelIndexableNode(baseReg: Int, propertyReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"del ${reg(baseReg)}[${reg(propertyReg)}]\t(DelIndexableNode)"
}
case class DelPropertyNode(baseReg: Int, property: String, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"del ${reg(baseReg)}.$property\t(DelPropertyNode)"
}

// No Operation node; pass
case class NoOpNode(note: String, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"$note\t(NoOpNode)"
}

// Break node
case class BreakNode(note: String, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"$note\t(BreakNode)"
}

// If statement: if (condition): then_block else: else_block
case class IfNode(conditionReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"if ${reg(conditionReg)}\t(IfNode)"
}

// Return statement: return result
case class ReturnNode(resultReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"return ${reg(resultReg)}\t(ReturnNode)"
}

case class ExceptionalExitNode(name: String, entryNode: Node, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"ExceptionalExitNode($name)"
}

// Function invokation and Object creation calls; result = [base.]function(arguments)
case class CallNode(functionReg: Int, argRegs: List[Int], keywordRegs: Map[String, Int] = Map(), starArgReg: Option[Int] = None, kwArgReg: Option[Int] = None, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString() = {
    val args = ASTPrettyPrinter.implodeStringList(argRegs.map((argReg) => reg(argReg)), ", ")
    val kwargs = ASTPrettyPrinter.implodeStringList(keywordRegs.toList.map((entry) => entry._1 + "=" + reg(entry._2)), ", ", true)
    val starargs = starArgReg match { case Some(arg) => "*" + reg(arg) case None => "" }
    val keywords = kwArgReg match { case Some(arg) => "**" + reg(arg) case None => "" }
    
    val mixed_args = ASTPrettyPrinter.implodeStringList(scala.List(args, kwargs, starargs, keywords), ", ", true)
    s"_ = ${reg(functionReg)}($mixed_args)\t(CallNode)"
  }
}

case class AfterCallNode(resultReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString() = {
    s"${reg(resultReg)} = _\t(AfterCallNode)"
  }
}

// Raise error; Raise value
// If a raiseNode happens without any valueReg its a reRaise and should not change anything to the Trace etc.
case class RaiseNode(valueReg: Option[Int], id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = valueReg match {
    case Some(valueReg) => s"raise ${reg(valueReg)}\t(RaiseNode)"
    case None => s"raise\t(RaiseNode)"
  }
}

// Except an exception; except [(]types[)] [as result]: 
case class ExceptNode(types: List[String], names: List[String], id: UUID = UUID.randomUUID()) extends Node(id) {
  val typesStr = types.foldLeft("")((acc,s) => if (acc == "") s else acc + ", " + s)
  val namesStr = names.foldLeft("")((acc,s) => if (acc == "") s else acc + ", " + s)
  override def toString = s"except: $typesStr as $namesStr"
}

// Binary operation; result = arg1 op arg2
case class CompareOpNode(op: cmpopType, arg1Reg: Int, arg2Reg: Int, resultReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = ${reg(arg1Reg)} ${ASTPrettyPrinter.cmpopTypeToString(op)} ${reg(arg2Reg)}\t(BinOpNode)"
}
case class BinOpNode(op: operatorType, arg1Reg: Int, arg2Reg: Int, resultReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = ${reg(arg1Reg)} ${ASTPrettyPrinter.operatorToString(op)} ${reg(arg2Reg)}\t(BinOpNode)"
}
case class UnOpNode(op: unaryopType, arg1Reg: Int, resultReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = ${ASTPrettyPrinter.unaryopTypeToString(op)} ${reg(arg1Reg)}\t(UnOpNode)"
}

// Print node; print value
case class PrintNode(dest: Option[Int], valueRegs: List[Int], id: UUID = UUID.randomUUID()) extends Node(id) {
  val arg_string = valueRegs.foldLeft("")((acc,s) => if (acc == "") reg(s) else acc + ", " + reg(s))
  override def toString = s"print $arg_string\t(Print)"
}

// Misc
case class GlobalNode(variable: String, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"Gloabal ${variable}\t(GlobalNode)"
}

case class AssertIterable(reg: Int, length: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"assert(${reg(reg)}.length = $length)"
}

case class ImportNode(names: List[String], isImplicit: Boolean = false, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"import ${ASTPrettyPrinter.implodeStringList(names, ".", false)}"
}

case class AssertNode(reg: Int, negate: Boolean = false, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = negate match {
    case true => s"assertNot(${reg(reg)})"
    case false => s"assert(${reg(reg)})"
  }
}