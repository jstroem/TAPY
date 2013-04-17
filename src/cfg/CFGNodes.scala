package tapy.cfg

import tapy.constants

abstract class Node(label: String)

// Write variable; variable = value
case class WriteVariableNode(variable:String,value_reg:Int, label: String) extends Node(label)

// Read variable; result = variable
case class ReadVariableNode(variable:String,result_reg:Int, label: String) extends Node(label)

// Write property into base (object/class); base.property = value
case class WritePropertyNode(base_reg:Int,property:String,value_reg:Int, label: String) extends Node(label)

// Read property from base (object/class); result =  base.property
case class ReadPropertyNode(base_reg:Int,property:String,result_reg:Int, label: String) extends Node(label)

// Write property into dictionary: base[property] = value
case class WriteDictionaryNode(base_reg:Int,property_reg:Int,value_reg:Int, label: String) extends Node(label)

// Read property from dictionary: result = base[property]
case class ReadDictionaryNode(base_reg:Int,property_reg:Int,result_reg:Int, label: String) extends Node(label)

// No Operation node; pass
case class NoOpNode(label: String) extends Node(label)

// If statement: if (condition): then_block else: else_block
case class IfNode(condition_reg:Int,then_block:Node,else_block:Node, label: String) extends Node(label)

// Sequence of statements
// case class BasicBlockNode(nodes:List[Node], label: String) extends Node(label)

// Return statement: return result
case class ReturnNode(result_reg:Int, label: String) extends Node(label)

// Exceptional Return; 
case class ExceptionalReturnNode(label: String) extends Node(label)

// Function invokation and Object creation calls; result = [base.]function(arguments)
case class CallNode(result_reg:Int,base_reg:Option[Int],function_reg:Int,argument_regs:List[Int], label: String) extends Node(label)

// Raise error; Raise value
case class RaiseNode(value_reg:Int, label: String) extends Node(label)

// Except an exception; except [(]types[)] [as result]:
case class ExceptNode(types:List[String],result_reg:Option[Int], label: String) extends Node(label)

// Binary operation; result = arg1 op arg2
case class BinOpNode(op: constants.BinOp,arg1_reg:Int,arg2_reg:Int,result_reg:Int, label: String) extends Node(label)

// Unary operation: result = op arg1
case class UnaryOpNode(op: constants.UnOp,arg1_reg:Int,result_reg:Int, label: String) extends Node(label)

// Print node; print value
case class PrintNode(value_reg: Int, label: String) extends Node(label)

// TODO
case class ForInNode(label: String) extends Node(label)