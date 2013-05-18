package tapy.constants

object UnOp extends Enumeration {
  type UnOp = Value
  
  //  not, ~,     -,     +
  val NOT, TILDE, MINUS, PLUS  = Value
}

object BinOp extends Enumeration {
  type BinOp = Value
  
  //  +     -      <=   >=   <   >   ==  !=   in  is, *     /    %    //    >>   <<   &    ^    |   **
  val PLUS, MINUS, LTE, GTE, LT, GT, EQ, NEQ, IN, IS, MULT, DIV, MOD, IDIV, SHR, SHL, AND, XOR, OR, POW = Value
}

object BoolOp extends Enumeration {
  type BoolOp = Value
  
  //  and  or
  val AND, OR = Value
}

object StackConstants extends Enumeration {
	type StackConstants = Int

	val RETURN = -1
	val RETURN_CONSTRUCTOR = -2
	val EXCEPTION = -3
	val TRACE = -4
	val EXCEPTION_TYPE = -5
}