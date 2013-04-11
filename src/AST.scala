package TAPY

object AST {
  // NAME
  case class Name(name:String)

  // dotted_name: identifier ('.' identifier)*
  case class DottedName(names:List[Name])
  // '.' | '...'
  case class Dot()

  // ('.' | '...')+
  case class Dots(dots:List[Dot]) extends DottedNameOrDots

  // *
  case class Star() extends ImportAsNamesOrStar

  // stmt: simple_stmt | compound_stmt
	sealed trait Statement

  //simple_stmt: small_stmt (';' small_stmt)* [';'] NEWLINE
  case class SimpleStatement(smallStmts: List[SmallStatement]) extends Statement

  //small_stmt: (expr_stmt | del_stmt | pass_stmt | flow_stmt | import_stmt | global_stmt | nonlocal_stmt | assert_stmt)
  sealed trait SmallStatement

  //expr_stmt: testlist_star_expr augassign_yield_expr_testlist_or_assign_yield_expr_testlist_star_expr_list
  case class ExpressionStatement(testlist_star_expr:TestlistStarExpression,augassign_yield:AugYieldtestlistOrAssignYieldTestlistStarList) extends SmallStatement

  // test_star_expression: test | star_expr
  sealed trait TestStarExpression

  // testlist_star_expr: test_star_expression (',' test_star_expression)* [',']
  case class TestlistStarExpression(test_star_expr:List[TestStarExpression])

  // yield_expr_testlist: yield_expr | testlist
  sealed trait YieldTestlist

  // augassign_yield_expr_testlist_or_assign_yield_expr_testlist_star_expr_list: augassign_yield_expr_testlist | assign_yield_expr_testlist_star_expr_list
  sealed trait AugYieldtestlistOrAssignYieldTestlistStarList

  // augassign_yield_expr_testlist: augassign yield_expr_testlist
  case class AugYieldtestlist(augassign:AugAssign,yieldtestlist:YieldTestlist) extends AugYieldtestlistOrAssignYieldTestlistStarList

  // yield_expr_testlist_star_expr: yield_expr | testlist_star_expr
  sealed trait YieldTestlistStar

  // assign_yield_expr_testlist_star_expr: '=' yield_expr_testlist_star_expr
  case class AssignYieldTestlistStar(yieldtestlist:YieldTestlistStar)

  // assign_yield_expr_testlist_star_expr_list: assign_yield_expr_testlist_star_expr*
  case class AssignYieldTestlistStarList(yieldtestlists:List[YieldTestlistStar]) extends AugYieldtestlistOrAssignYieldTestlistStarList

  // del_stmt: 'del' exprlist
	case class DelStatement(exprs:List[Expression]) extends SmallStatement

  // pass_stmt: 'pass'
	case class PassStatement() extends SmallStatement

  // flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt
	sealed trait FlowStatement extends SmallStatement

  // break_stmt: 'break'
	case class BreakStatement() extends FlowStatement

  // continue_stmt: 'continue'
	case class ContinueStatement() extends FlowStatement

  // return_stmt: 'return' [testlist]
	case class ReturnStatement(otests:Option[List[Test]]) extends FlowStatement

  // raise_stmt: 'raise' [test ['from' test]]
	case class RaiseStatement(otests:Option[Pair[Test,Option[Test]]]) extends FlowStatement

  // yield_stmt: yield_expr
	case class YieldStatement(yield_expr:YieldExpression) extends FlowStatement

  // import_stmt: import_name | import_from
  sealed trait ImportStatement extends SmallStatement

  // import_name: 'import' dotted_as_names
  case class ImportName(dotted_as_names:DottedAsNames) extends ImportStatement

  // ('*' | import_as_names
  sealed trait ImportAsNamesOrStar

  sealed trait DottedNameOrDots

  case class DotDottedName(dots:List[Dot],dotted_name:DottedName) extends DottedNameOrDots

  case class ImportFrom(dotted_name:DottedNameOrDots, import_as_names:ImportAsNamesOrStar) 

  // import_as_name: NAME ['as' NAME]
  case class ImportAsName(name:Name,oname:Option[Name])

  // dotted_as_name: dotted_name ['as' NAME]
  case class DottedAsName(dotted_name:DottedName,oname:Option[Name])

  // import_as_names: import_as_name (',' import_as_name)* [',']
  case class ImportAsNames(import_as_names:List[ImportAsName]) extends ImportAsNamesOrStar

  // dotted_as_names: dotted_as_name (',' dotted_as_name)*
  case class DottedAsNames(dotted_as_names:List[DottedAsName]) extends ImportStatement

  //global_stmt: 'global' NAME (',' NAME)*
  case class GlobalStatement(names:List[Name]) extends SmallStatement

  //nonlocal_stmt: 'nonlocal' NAME (',' NAME)*
  case class NonLocalStatement(names:List[Name]) extends SmallStatement

  //assert_stmt: 'assert' test [',' test]
  case class AssertStatement(tests:List[Test]) extends SmallStatement

  // augassign: ('+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^=' | '<<=' | '>>=' | '**=' | '//=')
	case class AugAssign(aug:String)


  //TODO
  sealed trait Expression
  case class Test()
  case class YieldExpression() extends Expression with YieldTestlist with YieldTestlistStar
}


/*
single_input: NEWLINE | simple_stmt | compound_stmt NEWLINE
file_input: (NEWLINE | stmt)* ENDMARKER
eval_input: testlist NEWLINE* ENDMARKER

decorator: '@' dotted_name [ '(' [arglist] ')' ] NEWLINE
decorators: decorator+
decorated: decorators (classdef | funcdef)
funcdef: 'def' NAME parameters ['->' test] ':' suite
parameters: '(' [typedargslist] ')'
typedargslist: (tfpdef ['=' test] (',' tfpdef ['=' test])* [','
       ['*' [tfpdef] (',' tfpdef ['=' test])* [',' '**' tfpdef] | '**' tfpdef]]
     |  '*' [tfpdef] (',' tfpdef ['=' test])* [',' '**' tfpdef] | '**' tfpdef)
tfpdef: NAME [':' test]
varargslist: (vfpdef ['=' test] (',' vfpdef ['=' test])* [','
       ['*' [vfpdef] (',' vfpdef ['=' test])* [',' '**' vfpdef] | '**' vfpdef]]
     |  '*' [vfpdef] (',' vfpdef ['=' test])* [',' '**' vfpdef] | '**' vfpdef)
vfpdef: NAME

# For normal assignments, additional restrictions enforced by the interpreter

compound_stmt: if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | classdef | decorated
if_stmt: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
while_stmt: 'while' test ':' suite ['else' ':' suite]
for_stmt: 'for' exprlist 'in' testlist ':' suite ['else' ':' suite]
try_stmt: ('try' ':' suite
           ((except_clause ':' suite)+
            ['else' ':' suite]
            ['finally' ':' suite] |
           'finally' ':' suite))
with_stmt: 'with' with_item (',' with_item)*  ':' suite
with_item: test ['as' expr]
# NB compile.c makes sure that the default except clause is last
except_clause: 'except' [test ['as' NAME]]
suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT

test: or_test ['if' or_test 'else' test] | lambdef
test_nocond: or_test | lambdef_nocond
lambdef: 'lambda' [varargslist] ':' test
lambdef_nocond: 'lambda' [varargslist] ':' test_nocond
or_test: and_test ('or' and_test)*
and_test: not_test ('and' not_test)*
not_test: 'not' not_test | comparison
comparison: expr (comp_op expr)*
# <> isn't actually a valid comparison operator in Python. It's here for the
# sake of a __future__ import described in PEP 401
comp_op: '<'|'>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not'
star_expr: '*' expr
expr: xor_expr ('|' xor_expr)*
xor_expr: and_expr ('^' and_expr)*
and_expr: shift_expr ('&' shift_expr)*
shift_expr: arith_expr (('<<'|'>>') arith_expr)*
arith_expr: term (('+'|'-') term)*
term: factor (('*'|'/'|'%'|'//') factor)*
factor: ('+'|'-'|'~') factor | power
power: atom trailer* ['**' factor]
atom: ('(' [yield_expr|testlist_comp] ')' |
       '[' [testlist_comp] ']' |
       '{' [dictorsetmaker] '}' |
       NAME | NUMBER | STRING+ | '...' | 'None' | 'True' | 'False')
testlist_comp: (test|star_expr) ( comp_for | (',' (test|star_expr))* [','] )
trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME
subscriptlist: subscript (',' subscript)* [',']
subscript: test | [test] ':' [test] [sliceop]
sliceop: ':' [test]
exprlist: (expr|star_expr) (',' (expr|star_expr))* [',']
testlist: test (',' test)* [',']
dictorsetmaker: ( (test ':' test (comp_for | (',' test ':' test)* [','])) |
                  (test (comp_for | (',' test)* [','])) )

classdef: 'class' NAME ['(' [arglist] ')'] ':' suite

arglist: (argument ',')* (argument [',']
                         |'*' test (',' argument)* [',' '**' test] 
                         |'**' test)
# The reason that keywords are test nodes instead of NAME is that using NAME
# results in an ambiguity. ast.c makes sure it's a NAME.
argument: test [comp_for] | test '=' test  # Really [keyword '='] test
comp_iter: comp_for | comp_if
comp_for: 'for' exprlist 'in' or_test [comp_iter]
comp_if: 'if' test_nocond [comp_iter]

# not used in grammar, but may appear in "node" passed from Parser to Compiler
encoding_decl: NAME

yield_expr: 'yield' [yield_arg]
yield_arg: 'from' test | testlist*/