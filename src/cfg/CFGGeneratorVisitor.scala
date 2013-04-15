package tapy.cfg

import org.python.antlr.ast._;

object CFGGeneratorVisitor extends VisitorIF[ControlFlowGraph] {
  def visitModule(node: Module): ControlFlowGraph = { return null; }
  
  def visitInteractive(node: Interactive): ControlFlowGraph = { return null; }
  
  def visitExpression(node: Expression): ControlFlowGraph = { return null; }
  
  def visitSuite(node: Suite): ControlFlowGraph = { return null; }
  
  def visitFunctionDef(node: FunctionDef): ControlFlowGraph = { return null; }
  
  def visitClassDef(node: ClassDef): ControlFlowGraph = { return null; }
  
  def visitReturn(node: Return): ControlFlowGraph = { return null; }
  
  def visitDelete(node: Delete): ControlFlowGraph = { return null; }
  
  def visitAssign(node: Assign): ControlFlowGraph = { return null; }
  
  def visitAugAssign(node: AugAssign): ControlFlowGraph = { return null; }
  
  def visitPrint(node: Print): ControlFlowGraph = { return null; }
  
  def visitFor(node: For): ControlFlowGraph = { return null; }
  
  def visitWhile(node: While): ControlFlowGraph = { return null; }
  
  def visitIf(node: If): ControlFlowGraph = { return null; }
  
  def visitWith(node: With): ControlFlowGraph = { return null; }
  
  def visitRaise(node: Raise): ControlFlowGraph = { return null; }
  
  def visitTryExcept(node: TryExcept): ControlFlowGraph = { return null; }
  
  def visitTryFinally(node: TryFinally): ControlFlowGraph = { return null; }
  
  def visitAssert(node: Assert): ControlFlowGraph = { return null; }
  
  def visitImport(node: Import): ControlFlowGraph = { return null; }
  
  def visitImportFrom(node: ImportFrom): ControlFlowGraph = { return null; }
  
  def visitExec(node: Exec): ControlFlowGraph = { return null; }
  
  def visitGlobal(node: Global): ControlFlowGraph = { return null; }
  
  def visitExpr(node: Expr): ControlFlowGraph = { return null; }
  
  def visitPass(node: Pass): ControlFlowGraph = { return null; }
  
  def visitBreak(node: Break): ControlFlowGraph = { return null; }
  
  def visitContinue(node: Continue): ControlFlowGraph = { return null; }
  
  def visitBoolOp(node: BoolOp): ControlFlowGraph = { return null; }
  
  def visitBinOp(node: BinOp): ControlFlowGraph = { return null; }
  
  def visitUnaryOp(node: UnaryOp): ControlFlowGraph = { return null; }
  
  def visitLambda(node: Lambda): ControlFlowGraph = { return null; }
  
  def visitIfExp(node: IfExp): ControlFlowGraph = { return null; }
  
  def visitDict(node: Dict): ControlFlowGraph = { return null; }
  
  def visitSet(node: Set): ControlFlowGraph = { return null; }
  
  def visitListComp(node: ListComp): ControlFlowGraph = { return null; }
  
  def visitSetComp(node: SetComp): ControlFlowGraph = { return null; }
  
  def visitDictComp(node: DictComp): ControlFlowGraph = { return null; }
  
  def visitGeneratorExp(node: GeneratorExp): ControlFlowGraph = { return null; }
  
  def visitYield(node: Yield): ControlFlowGraph = { return null; }
  
  def visitCompare(node: Compare): ControlFlowGraph = { return null; }
  
  def visitCall(node: Call): ControlFlowGraph = { return null; }
  
  def visitRepr(node: Repr): ControlFlowGraph = { return null; }
  
  def visitNum(node: Num): ControlFlowGraph = { return null; }
  
  def visitStr(node: Str): ControlFlowGraph = { return null; }
  
  def visitAttribute(node: Attribute): ControlFlowGraph = { return null; }
  
  def visitSubscript(node: Subscript): ControlFlowGraph = { return null; }
  
  def visitName(node: Name): ControlFlowGraph = { return null; }
  
  def visitList(node: List): ControlFlowGraph = { return null; }
  
  def visitTuple(node: Tuple): ControlFlowGraph = { return null; }
  
  def visitEllipsis(node: Ellipsis): ControlFlowGraph = { return null; }
  
  def visitSlice(node: Slice): ControlFlowGraph = { return null; }
  
  def visitExtSlice(node: ExtSlice): ControlFlowGraph = { return null; }
  
  def visitIndex(node: Index): ControlFlowGraph = { return null; }
  
  def visitExceptHandler(node: ExceptHandler): ControlFlowGraph = { return null; }
}