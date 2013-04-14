package TAPY

object UnOp extends Enumeration {
	//	not, ~ 
	val NOT, TILDE  = Value

}

object BinOp extends Enumeration {
	//  +     -      <=   >=   <   >   ==  !=   in  is, *     /    %    //    >>   <<   &    ^    |   **
	val PLUS, MINUS, LTE, GTE, LT, GT, EQ, NEQ, IN, IS, MULT, DIV, MOD, IDIV, SHR, SHL, AND, XOR, OR, POW = Value
}