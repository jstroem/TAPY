package TAPY

object Operator {
	class UnOp extends Enumeration {
		//	not, ~ 
		val NOT, TILDE  = Value

	}

	class BinOp extends Enumeration {
		//  +     -      <=   >=   <   >   ==  !=   in  is, *     /    %    //    >>   <<   &    ^    |   **
		val PLUS, MINUS, LTE, GTE, LT, GT, EQ, NEQ, IN, IS, MULT, DIV, MOD, IDIV, SHR, SHL, AND, XOR, OR, POW = Value
	}
}