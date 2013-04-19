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
