package TAPY

object CFG {
	sealed trait Node

	// Write variable; variable = value
	case class WriteVariableNode(variable:String,value_reg:Int) extends Node

	// Read variable; result = variable
	case class ReadVariableNode(variable:String,result_reg:Int) extends Node

	// Write property into base (object/class); base.property = value
	case class WritePropertyNode(base_reg:Int,property:String,value_reg:Int) extends Node

	// Read property from base (object/class); result =  base.property
	case class ReadPropertyNode(base_reg:Int,property:String,result_reg:Int) extends Node

	// Write property into dictionary: base[property] = value
	case class WriteDictionaryNode(base_reg:Int,property_reg:Int,value_reg:Int) extends Node

	// Read property from dictionary: result = base[property]
	case class ReadDictionaryNode(base_reg:Int,property_reg:Int,result_reg:Int) extends Node

	// No Operation node; pass
	case class NoOpNode() extends Node

	// If statement: if (condition): then_block else: else_block
	case class IfNode(condition_reg:Int,then_block:BasicBlockNode,else_block:BasicBlockNode) extends Node

	// Sequence of statements
	case class BasicBlockNode(nodes:List[Node]) extends Node

	// Return statement: return result
	case class ReturnNode(result_reg:Int) extends Node

	// Exceptional Return; 
	case class ExceptionalReturnNode() extends Node

	// Function invokation and Object creation calls; result = [base.]function(arguments)
	case class CallNode(result_reg:Int,base_reg:Option[Int],function_reg:Int,argument_regs:List[Int]) extends Node

	// Raise error; Raise value
	case class RaiseNode(value_reg:Int) extends Node

	// Except an exception; except [(]types[)] [as result]:
	case class ExceptNode(types:List[String],result_reg:Option[Int]) extends Node

	// Binary operation; result = arg1 op arg2
	case class BinOpNode(op:Operator.BinOp,arg1_reg:Int,arg2_reg:Int,result_reg:Int) extends Node

	// Unary operation: result = op arg1
	case class UnaryOpNode(op:Operator.UnOp,arg1_reg:Int,result_reg:Int) extends Node

	// TODO
	case class ForInNode() extends Node



}