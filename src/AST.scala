package TAPY

object AST {
  // NAME
  case class Name(name: String) extends Atom
  
  // dotted_name: identifier ('.' identifier)*
  case class DottedName(names: List[Name])
  
  // '.'
  case class Dot() extends DotOrTripleDot
  
  // '...'
  case class TripleDot() extends DotOrTripleDot with Atom
  
  // ('.' | '...')+
  case class Dots(dots: List[DotOrTripleDot]) extends DottedNameOrDots

  sealed trait DotOrTripleDot
  
  // *
  case class Star() extends ImportAsNamesOrStar
  
  
  /**
    * Statements
    */
  
  // stmt: simple_stmt | compound_stmt
  sealed trait Statement
  sealed trait SimpleStmOrStms
  
  case class StatementList(stms: List[Statement]) extends SimpleStmOrStms
  
  
  /**
    * Simple statement
    */
  
  // simple_stmt: small_stmt (';' small_stmt)* [';'] NEWLINE
  case class SimpleStatement(smallStmts: List[SmallStatement]) extends Statement with SimpleStmOrStms
  
  
  /**
    * Small statement
    */
  
  // small_stmt: (expr_stmt | del_stmt | pass_stmt | flow_stmt | import_stmt | global_stmt | nonlocal_stmt | assert_stmt)
  sealed trait SmallStatement
  
  
  /**
    * Small statement: Expression statement
    */
  
  // expr_stmt: testlist_star_expr augassign_or_assign
  case class ExpressionStatement(testOrStarExpList: List[TestOrStarExpression], augAssignOrAssign: AugAssignOrAssign) extends SmallStatement
  
  // test_or_star_expression: test | star_expr
  sealed trait TestOrStarExpression

  // augassign_or_assign: augassign_yield_expr_testlist | assign_yield_expr_testlist_star_expr_list
  sealed trait AugAssignOrAssign

  // augassign_yield_expr_testlist: augassign yield_expr_testlist
  case class AugAssign(augAssignOp: AugAssignOp, yieldExpOrTestList: YieldExpressionOrTestList) extends AugAssignOrAssign
  
  // augassign: ('+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^=' | '<<=' | '>>=' | '**=' | '//=')
  case class AugAssignOp(op: String)
  
  // yield_expr_testlist: yield_expr | testlist
  sealed trait YieldExpressionOrTestList

  // assign_yield_expr_testlist_star_expr_list: assign_yield_expr_testlist_star_expr*
  case class Assign(yieldExpOrTestOrStarExpListList: List[YieldExpressionOrTestOrStarExpressionList]) extends AugAssignOrAssign

  // yield_expr_testlist_star_expr: yield_expr | testlist_star_expr
  sealed trait YieldExpressionOrTestOrStarExpressionList
  
  case class TestOrStarExpressionList(testOrStarExps: List[TestOrStarExpression]) extends YieldExpressionOrTestOrStarExpressionList with ForComprehensionOrTestOrStarExpressionList
  
  
  /**
    * More small statements
    */
  
  // del_stmt: 'del' exprlist
	case class DeleteStatement(exps: List[Expression]) extends SmallStatement

  // pass_stmt: 'pass'
	case class PassStatement() extends SmallStatement

  // flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt
	sealed trait FlowStatement extends SmallStatement

  // break_stmt: 'break'
	case class BreakStatement() extends FlowStatement

  // continue_stmt: 'continue'
	case class ContinueStatement() extends FlowStatement

  // return_stmt: 'return' [testlist]
	case class ReturnStatement(tests: Option[List[Test]]) extends FlowStatement

  // raise_stmt: 'raise' [test ['from' test]]
	case class RaiseStatement(tests: Option[Pair[Test, Option[Test]]]) extends FlowStatement

  // yield_stmt: yield_expr
	case class YieldStatement(yieldExp: YieldExpression) extends FlowStatement

	
	/**
	  * Small statement: Import statement
	  */
	
	// TODO: Check this section
	
  // import_stmt: import_name | import_from
  sealed trait ImportStatement extends SmallStatement

  // import_name: 'import' dotted_as_names
  case class ImportName(dottedAsNames: DottedAsNames) extends ImportStatement
  
  case class ImportFrom(dottedName: DottedNameOrDots, importAsNames: ImportAsNamesOrStar) extends ImportStatement

  // ('*' | import_as_names
  sealed trait ImportAsNamesOrStar

  sealed trait DottedNameOrDots

  case class DotDottedName(dots: List[DotOrTripleDot], dottedName: DottedName) extends DottedNameOrDots

  // import_as_name: NAME ['as' NAME]
  case class ImportAsName(name:Name,oname:Option[Name])

  // dotted_as_name: dotted_name ['as' NAME]
  case class DottedAsName(dotted_name:DottedName,oname:Option[Name])

  // import_as_names: import_as_name (',' import_as_name)* [',']
  case class ImportAsNames(import_as_names:List[ImportAsName]) extends ImportAsNamesOrStar

  // dotted_as_names: dotted_as_name (',' dotted_as_name)*
  case class DottedAsNames(dotted_as_names:List[DottedAsName]) extends ImportStatement

  
  /**
    * More small statements
    */
  
  //global_stmt: 'global' NAME (',' NAME)*
  case class GlobalStatement(names: List[Name]) extends SmallStatement

  //nonlocal_stmt: 'nonlocal' NAME (',' NAME)*
  case class NonLocalStatement(names: List[Name]) extends SmallStatement

  //assert_stmt: 'assert' test [',' test]
  case class AssertStatement(tests: List[Test]) extends SmallStatement

  
  /**
    * Compound statement
    */
  
  // compound_stmt: if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | classdef | decorated
	sealed trait CompoundStatement extends Statement
	
	// if_stmt: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
  case class IfStatement(test: Test, suite: Suite, elif: List[Pair[Test, Suite]], el: Option[Suite]) extends CompoundStatement 
  
  // suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT
  case class Suite(simpleStmOrStms: SimpleStmOrStms)
  
  // while_stmt: 'while' test ':' suite ['else' ':' suite]
  case class WhileStatement(test: Test, suite: Suite, el: Option[Suite]) extends CompoundStatement
  
	// for_stmt: 'for' exprlist 'in' testlist ':' suite ['else' ':' suite]
  case class ForStatement(expList: ExpressionList, in: List[Test], suite: Suite, el: Option[Suite]) extends CompoundStatement
  
  // try_stmt: ('try' ':' suite final_or_except_final)
  case class TryStatement(suite: Suite, finalOrExceptFinal: FinalOrExceptFinal) extends CompoundStatement
	
  // final_or_except_final: final | except_final
  sealed trait FinalOrExceptFinal
  
  // final: 'finally' ':' suite)
  case class Final(suite: Suite) extends FinalOrExceptFinal
  
  // except_final: (except_clause ':' suite)+ ['else' ':' suite] ['finally' ':' suite]
  case class ExceptFinal(excepts: List[Pair[ExceptClause, Suite]], elSuite: Suite, finallySuite: Suite) extends FinalOrExceptFinal
  
  // except_clause: 'except' [test ['as' NAME]]
  case class ExceptClause(exceptTest: Option[Pair[Test, Option[Name]]])
  
  // with_stmt: 'with' with_item (',' with_item)*  ':' suite
  case class WithStatement(withItems: List[WithItem], suite: Suite) extends CompoundStatement
  
  // with_item: test ['as' expr]
  case class WithItem(test: Test, as: Expression)
  
  // class_or_func_def: classdef | funcdef
  sealed trait ClassOrFunctionDef
  
  // classdef: 'class' NAME ['(' [arglist] ')'] ':' suite
  case class ClassDef(name: Name, arguments: Option[ArgumentList], suite: Suite) extends CompoundStatement with ClassOrFunctionDef
  
  // funcdef: 'def' NAME parameters ['->' test] ':' suite
  case class FunctionDef(name: Name, parameters: List[TypedArgument], test: Option[Test], suite: Suite) extends CompoundStatement with ClassOrFunctionDef
  
  // decorated: decorators (classdef | funcdef)
  case class Decorated(decorators: List[Decorator], classOrFuncDef: ClassOrFunctionDef) extends CompoundStatement
  
  // decorator: '@' dotted_name [ '(' [arglist] ')' ] NEWLINE
  case class Decorator(dottedName: DottedName, arguments: Option[ArgumentList])
  
  
  /**
    * Tests and comparisons (?)
    */
  
  //test: or_else_or_lambda
  case class Test(e: OrElseTestOrLambdaDef) extends TestOrStarExpression
  
  // or_else_or_lambda: or_else | lambdef
  sealed trait OrElseTestOrLambdaDef
  
  // or_else: or_test ['if' or_test 'else' test]
  case class OrElseTest(or: OrTest, elseTest: Option[Pair[OrTest, Test]]) extends OrElseTestOrLambdaDef
  
  // or_test: and_test ('or' and_test)*
  case class OrTest(test: List[AndTest]) extends OrTestOrLambdaDefNoCond
  
  // and_test: not_test ('and' not_test)*
  case class AndTest(notTests: List[NotTest])
  
  // not_test: not_test_or_comparison
  case class NotTest(notTestOrComparison: NotTestOrComparison) extends NotTestOrComparison

  // not_test_or_comparison: 'not' not_test | comparison
  sealed trait NotTestOrComparison
  
  // comparison: expr (comp_op expr)*
  case class Comparison(exp: Expression, compOpExps: List[Pair[CompOp, Expression]]) extends NotTestOrComparison
  
  // comp_op: '<'|'>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not'
  case class CompOp(op: String)
  
  //test_nocond: or_test | lambdef_nocond
  case class TestNoCond(orTestOrLambdaDefNoCond: OrTestOrLambdaDefNoCond)
  
  sealed trait OrTestOrLambdaDefNoCond
  
  
  /**
    * Expression
    */
  
  sealed trait ExpressionOrStarExpression
  
  // expr: xor_expr ('|' xor_expr)*
  case class Expression(xorExps: List[XOrExpression]) extends ExpressionOrStarExpression
  
  // xor_expr: and_expr ('^' and_expr)*
  case class XOrExpression(andExps: List[AndExpression])
  
  // and_expr: shift_expr ('&' shift_expr)*
  case class AndExpression(shiftExps: List[ShiftExpression])
  
  // shift_expr: arith_expr (arith_op arith_expr)*
  case class ShiftExpression(arithExp: ArithExpression, arithExps: List[Pair[ShiftOp, ArithExpression]])
  
  // shift_op: '<<' | '>>'
  case class ShiftOp(op: String)
  
  // arith_expr: term (arith_op term)*
  case class ArithExpression(term: Term, terms: List[Pair[ArithOp, Term]])
  
  // arith_op: '+' | '-'
  case class ArithOp(op: String)
  
  // term: factor_or_power (term_op factor_or_power)*
  case class Term(factor: Factor, factors: List[Pair[TermOp, Factor]])
  
  // term_op: '*' | '/' | '%' | '//'
  case class TermOp(op: String)
  
  // factor_or_power: factor | power
  sealed trait FactorOrPower
  
  // factor: factor_op factor_or_power
  case class Factor(factorOp: FactorOp, factor: FactorOrPower) extends FactorOrPower
  
  // factor_op: '+' | '-' | '~'
  case class FactorOp(op: String)
  
  // power: atom trailer* ['**' factor]
  case class Power(atom: Atom, trailers: List[Trailer], factor: Option[Factor]) extends FactorOrPower
  
  /* atom:
   *   ( '(' [yield_expr|testlist_comp] ')'
   *     | '[' [testlist_comp] ']'
   *     | '{' [dictorsetmaker] '}'
   *     | NAME | NUMBER | STRING+ | '...' | 'None' | 'True' | 'False') */
  sealed trait Atom
  
  case class Number(n: String)
  case class Strings(ss: List[String])
  case class None() extends Atom
  case class Bool(b: Boolean) extends Atom
  
  sealed trait YieldExpressionOrTestListComp
  case class OptionYieldExpressionOrTestListComp(option: Option[YieldExpressionOrTestListComp]) extends Atom
  
  // testlist_comp: (test|star_expr) ( comp_for | (',' (test|star_expr))* [','] )
  case class TestListComp(testOrStarExp: TestOrStarExpression, compForOrTestStarExpList: ForComprehensionOrTestOrStarExpressionList) extends YieldExpressionOrTestListComp
  case class OptionTestListComp(option: Option[TestListComp]) extends Atom
  
  sealed trait ForComprehensionOrTestOrStarExpressionList
  
  // star_expr: '*' expr
  case class StarExpression(exp: Expression) extends ExpressionOrStarExpression with TestOrStarExpression
  
  // trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME
  case class Trailer()
  
  // subscriptlist: subscript (',' subscript)* [',']
  case class SubscriptList(subscripts: List[Subscript])
  
  // subscript: test | [test] ':' [test] [sliceop]
  case class Subscript()
  
  // sliceop: ':' [test]
  case class SliceOp(test: Option[Test])
  
  // exprlist: (expr|star_expr) (',' (expr|star_expr))* [',']
  case class ExpressionList(exps: List[ExpressionOrStarExpression])
  
  // testlist: test (',' test)* [',']
  case class TestList(tests: List[Test]) extends YieldExpressionOrTestList
  
  // dictorsetmaker: ( (test ':' test (comp_for | (',' test ':' test)* [','])) | (test (comp_for | (',' test)* [','])) )
  case class DictionaryOrSetMaker()
  case class OptionDictionaryOrSetMaker(option: DictionaryOrSetMaker) extends Atom
  
  
  /**
    * Lambda definitions:
    */
  
  // lambdef: 'lambda' [varargslist] ':' test
  case class LambdaDef(varArgs: List[VarArgument], test: Test) extends OrElseTestOrLambdaDef
  
  // lambdef_nocond: 'lambda' [varargslist] ':' test_nocond
  case class LambdaDefNoCond(varArgs: List[VarArgument], testNoCond: TestNoCond) extends OrTestOrLambdaDefNoCond
  
  
  /**
    * Yield expressions:
    */
  // yield_expr: 'yield' [yield_arg]
  case class YieldExpression() extends YieldExpressionOrTestList with YieldExpressionOrTestOrStarExpressionList with YieldExpressionOrTestListComp
  
  // yield_arg: 'from' test | testlist*/
  case class YieldArg()
  
  // parameters: '(' [typedargslist] ')'
  
  /* typedargslist:
   *   (tfpdef ['=' test] (',' tfpdef ['=' test])*
   *   [ ',' ['*' [tfpdef] (',' tfpdef ['=' test])* [',' '**' tfpdef] | '**' tfpdef]]
   *   | '*' [tfpdef] (',' tfpdef ['=' test])* [',' '**' tfpdef] | '**' tfpdef) */
  case class TypedArgument(stars: String, tfpDef: Option[TFPDef], default: Option[Test])
  
  // tfpdef: NAME [':' test]
  case class TFPDef(name: Name, test: Option[Test])
  
  
  /**
    * Varargslist, vfpdef
    */
  
  /* varargslist:
   *   (vfpdef ['=' test] (',' vfpdef ['=' test])* [','
   *   [ '*' [vfpdef] (',' vfpdef ['=' test])* [',' '**' vfpdef] | '**' vfpdef]]
   *   | '*' [vfpdef] (',' vfpdef ['=' test])* [',' '**' vfpdef] | '**' vfpdef) */
  case class VarArgument(stars: String, vfpDef: Option[VFPDef], default: Option[Test])
  
  // vfpdef: NAME
  case class VFPDef(name: Name)
  
  // argument: test [comp_for] | test '=' test  # Really [keyword '='] test
  case class Argument()
  
  /* arglist:
   *   (argument ',')* (
   *     argument [',']
   *     | '*' test (',' argument)* [',' '**' test] 
   *     | '**' test
   *   ) */
  case class ArgumentList()
  
  
  /**
    * Comprehensions
    */
  
  // comp_iter: comp_for | comp_if
  sealed trait ComprehensionIterator
  
  // comp_for: 'for' exprlist 'in' or_test [comp_iter]
  case class ForComprehension(exps: ExpressionList, orTest: OrTest, compIter: Option[ComprehensionIterator]) extends ComprehensionIterator with ForComprehensionOrTestOrStarExpressionList
  
  // comp_if: 'if' test_nocond [comp_iter]
  case class IfComprehension(testNoCond: TestNoCond, compIter: Option[ComprehensionIterator]) extends ComprehensionIterator
}