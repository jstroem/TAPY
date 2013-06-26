package tapy.cfg

import org.python.antlr._
import org.python.antlr.ast._
import org.python.antlr.base.excepthandler
import scala.collection.JavaConversions._
import scala.Boolean

object ASTPrettyPrinter extends VisitorBase[String] {
  def implodeList(lst: java.util.List[_ <: PythonTree], sep: String = "") : String = {
    val list = lst.toList
    return list.headOption match {
      case Some(e) => list.tail.foldLeft(e.accept(this))((acc,el) => acc + sep + el.accept(this))
      case None => ""
    }
  }
  
  def implodeStringList(list: scala.List[String], sep: String = "", ignoreEmpty: Boolean = false) : String = {
    return list.foldLeft("")((acc,s) => if (ignoreEmpty && s == "") acc else if (acc == "") s else acc + sep + s)
  }

  def cmpopTypeToString(cmpOp: cmpopType) : String = cmpOp match {
    case cmpopType.UNDEFINED => "undefined"
    case cmpopType.Eq => "=="
    case cmpopType.NotEq => "!="
    case cmpopType.Lt => "<"
    case cmpopType.LtE => "<="
    case cmpopType.Gt => ">"
    case cmpopType.GtE => ">="
    case cmpopType.Is => "is"
    case cmpopType.IsNot => "is not"
    case cmpopType.In => "in"
    case cmpopType.NotIn => "not in"
  }

  def unaryopTypeToString(unaryOp: unaryopType) : String = unaryOp match {
    case unaryopType.UNDEFINED => "undefined"
    case unaryopType.Invert => "~"
    case unaryopType.Not => "not"
    case unaryopType.UAdd => "+"
    case unaryopType.USub => "-"
  }

  def boolopTypeToString(boolOp: boolopType) : String = boolOp match {
    case boolopType.UNDEFINED => "undefined"
    case boolopType.And => "and"
    case boolopType.Or => "or"
  }

  def operatorToString(op: operatorType) : String = op match {
    case operatorType.UNDEFINED => "UNDEFINED"
    case operatorType.Add => "+"
    case operatorType.Sub => "-"
    case operatorType.Mult => "*"
    case operatorType.Div => "/"
    case operatorType.Mod => "%"
    case operatorType.Pow => "**"
    case operatorType.LShift => ">>"
    case operatorType.RShift => "<<"
    case operatorType.BitOr => "|"
    case operatorType.BitXor => "^"
    case operatorType.BitAnd => "&"
    case operatorType.FloorDiv => "//"
  }

  def visitArguments(node: arguments) : String = {
    val defs = node.getInternalDefaults()
    val args = node.getInternalArgs()
    val argsDefaults = args.toList.zipWithIndex.map({ case (arg,idx) => if (idx < args.length - defs.length) arg.accept(this); else arg.accept(this) + "=" + defs.get(idx-(args.length - defs.length)).accept(this);})
    return implodeStringList(argsDefaults, ", ")
  }

  def visitComprehension(node: comprehension) : String = {
    val target = node.getInternalTarget().accept(this)
    val iter = node.getInternalIter().accept(this)
    val ifs = if (!node.getInternalIfs().isEmpty()) node.getInternalIfs().toList.foldLeft(" ")((acc,node) => acc + " if " + node.accept(this)) else ""
    return s"for $target in $iter$ifs"
  }

  def visitExcepthandler(node: excepthandler) : String = node match {
    case node : ExceptHandler => visitExceptHandler(node)
    case _ => return "<except handler not implemented>"
  }
  
  var indent : String = ""
    
  def indent(line: String): String = indent + line
  def incIndent(): Unit = indent += "  "
  def decIndent(): Unit = indent = indent.replaceFirst("  ", "")
  
  /* Abstract methods from VisitorBase */
  
  override def traverse(node: PythonTree): Unit = {
    node.traverse(this);
  }
  
  override def unhandled_node(node: PythonTree): String = {
    return indent("<not implemented>")
  }

  /* Implementation of visitor methods: */
  
  override def visitModule(node: Module): String = {
    return implodeList(node.getInternalBody(), "\n")
  }
  
  override def visitInteractive(node: Interactive): String = {
    return indent("<interactive not implemented>")
  }
  
  override def visitExpression(node: Expression): String = {
    return indent("<expression not implemented>")
  }
  
  override def visitSuite(node: Suite): String = {
    return indent("<suite not implemented>")
  }
  
  override def visitFunctionDef(node: FunctionDef): String = {
    val decoratorList = node.getInternalDecorator_list().toList.map((e) => "@" + e.accept(this) + "\n")
    val decorator = implodeStringList(decoratorList, "")
    val name = node.getInternalName()
    val args = visitArguments(node.getInternalArgs())
    
    incIndent()
    val body = implodeList(node.getInternalBody(), "\n")
    decIndent()
    
    return indent(decorator + s"def $name($args):\n$body\n")
  }
  
  override def visitClassDef(node: ClassDef): String = {
    val name = node.getInternalName()
    val internal_bases = implodeList(node.getInternalBases(), ", ")
    
    incIndent()
    val body = implodeList(node.getInternalBody(), "\n")
    decIndent()

    return indent(s"class $name($internal_bases):\n$body")
  }
  
  override def visitReturn(node: Return): String = {
    if (node.getInternalValue() == null)
      return indent("return")
    else
      return indent("return " + node.getInternalValue().accept(this))
  }
  
  override def visitDelete(node: Delete): String = {
    return indent("del " + implodeList(node.getInternalTargets(), ", "))
  }
  
  override def visitAssign(node: Assign): String = {
    val targets = implodeList(node.getInternalTargets(), " = ")
    val value = node.getInternalValue().accept(this)
    return indent(s"$targets = $value")
  }
  
  override def visitAugAssign(node: AugAssign): String = {
    val target = node.getInternalTarget().accept(this)
    val operator = operatorToString(node.getInternalOp())
    val value = node.getInternalValue().accept(this)
    return indent(s"$target $operator= $value")
  }
  
  override def visitPrint(node: Print): String = {
    val dest = if (node.getInternalDest() != null) ">> " + node.getInternalDest().accept(this)+ ", " else ""
    val nl = if (node.getInternalNl()) "" else ","
    val values = implodeList(node.getInternalValues(), ", ")
    
    return indent(s"print $dest$values$nl")
  }
  
  override def visitFor(node: For): String = {
    val target = node.getInternalTarget().accept(this)
    val iter = node.getInternalIter().accept(this)
    incIndent()
    val else_branch = if (!node.getInternalOrelse().isEmpty()) implodeList(node.getInternalOrelse(), "\n") else ""
    val then_branch = implodeList(node.getInternalBody(), "\n")
    decIndent()
    if (node.getInternalOrelse().isEmpty())
      return indent(s"for $target in $iter:\n$then_branch")
    else
      return indent(s"for $target in $iter:\n$then_branch\n${indent}else:\n$else_branch")
  }
  
  override def visitWhile(node: While): String = {
    val test = node.getInternalTest().accept(this)

    incIndent()
    val else_branch = if (!node.getInternalOrelse().isEmpty()) implodeList(node.getInternalOrelse(), "\n") else ""
    val then_branch = implodeList(node.getInternalBody(), "\n")
    decIndent()

    if (node.getInternalOrelse().isEmpty())
      return indent(s"while $test:\n$then_branch")
    else
      return indent(s"while $test:\n$then_branch\n${indent}else:\n$else_branch")
  }
  
  override def visitIf(node: If): String = {
    val test = node.getInternalTest().accept(this)
    
    incIndent()
    val then_branch = implodeList(node.getInternalBody(), "\n")
    val else_branch = if (!node.getInternalOrelse().isEmpty()) implodeList(node.getInternalOrelse(), "\n") else ""
    decIndent()

    if (node.getInternalOrelse().isEmpty())
      return indent(s"if $test:\n$then_branch")
    else
      return indent(s"if $test:\n$then_branch\n${indent}else:\n$else_branch")
  }
  
  override def visitWith(node: With): String = {
    val ctx = node.getInternalContext_expr().accept(this)
    val opt = if (node.getInternalOptional_vars() != null) " as " + node.getInternalOptional_vars().accept(this) else ""
    incIndent()
    val body = implodeList(node.getInternalBody(), "\n")
    decIndent()
    return indent(s"with $ctx$opt:\n$body")
  }
  
  override def visitRaise(node: Raise) : String = {
    val itype = if (node.getInternalType() != null) node.getInternalType().accept(this) else ""
    val inst = if (node.getInternalInst() != null) node.getInternalInst().accept(this) else ""
    val tback = if (node.getInternalTback() != null) node.getInternalTback().accept(this) else ""
    return indent("raise " + implodeStringList(scala.List(itype, inst, tback), ", ", true))
  }
  
  override def visitTryExcept(node: TryExcept): String = {
    incIndent()
    val try_branch = implodeList(node.getInternalBody(), "\n")
    val else_branch = if (!node.getInternalOrelse().isEmpty()) implodeList(node.getInternalOrelse(), "\n") else ""
    decIndent()
    val excepters = if (!node.getInternalHandlers().isEmpty()) node.getInternalHandlers().toList.map((node) => visitExcepthandler(node)) else scala.List()
    val except = implodeStringList(excepters, "\n", true)
    if (node.getInternalOrelse().isEmpty())
      return implodeStringList(scala.List(indent(s"try:\n$try_branch"), except), "\n", true)
    else
      return implodeStringList(scala.List(indent(s"try:\n$try_branch"), except, indent("else:\n") + else_branch), "\n", true)
  }
  
  override def visitTryFinally(node: TryFinally): String = {
    incIndent()
    val body_branch = implodeList(node.getInternalBody(), "\n")
    val finally_branch = implodeList(node.getInternalFinalbody(), "\n")
    decIndent()

    return implodeStringList(scala.List(indent(s"try:\n$body_branch"), indent(s"finally:\n$finally_branch")), "\n", true)
  }
  
  override def visitAssert(node: Assert): String = {
    val test = node.getInternalTest().accept(this)
    val msg = if (node.getInternalMsg() != null) ", " + node.getInternalMsg().accept(this) else ""
    return indent(s"assert $test$msg")
  }
  
  override def visitImport(node: Import): String = {
    // println("visitImport");
    return "<not implemented>";
  }
  
  override def visitImportFrom(node: ImportFrom): String = {
    // println("visitImportFrom");
    return "<not implemented>";
  }
  
  override def visitExec(node: Exec): String = {
    // println("visitExec");
    val body = node.getInternalBody().accept(this)
    val globals = if (node.getInternalGlobals() != null) " in " + node.getInternalGlobals().accept(this) else ""
    val locals = if (node.getInternalLocals() != null) ", " + node.getInternalLocals().accept(this) else ""
    return indent(s"exec $body$globals$locals");
  }
  
  override def visitGlobal(node: Global): String = {
    // println("visitGlobal");
    return indent("global "+ implodeList(node.getInternalNameNodes(), ", "));
  }
  
  override def visitExpr(node: Expr): String = {
    // println("visitExpr");
    return indent(node.getInternalValue().accept(this));
  }
  
  override def visitPass(node: Pass): String = {
    // println("visitPass")
    return indent("pass")
  }
  
  override def visitBreak(node: Break): String = {
    // println("visitBreak")
    return indent("break")
  }
  
  override def visitContinue(node: Continue): String = {
    // println("visitContinue")
    return indent("continue")
  }
  
  override def visitBoolOp(node: ast.BoolOp): String = {
    // println("visitBoolOp")
    return "(" + implodeList(node.getInternalValues(), " " + boolopTypeToString(node.getInternalOp()) + " ") + ")"
  }
  
  override def visitBinOp(node: BinOp): String = {
    // println("visitBinOp");
    return "("+node.getInternalLeft.accept(this) + " " + operatorToString(node.getInternalOp) +" " + node.getInternalRight.accept(this)+")"
  }
  
  override def visitUnaryOp(node: UnaryOp): String = {
    // println("visitUnaryOp");
    return indent(unaryopTypeToString(node.getInternalOp()) + " " + node.getInternalOperand().accept(this))
  }
  
  override def visitLambda(node: Lambda): String = {
    // println("visitLambda");
    val attrs = visitArguments(node.getInternalArgs())
    val body = node.getInternalBody().accept(this)
    return s"lambda $attrs: $body";

  }
  
  override def visitIfExp(node: IfExp): String = {
    val test = node.getInternalTest().accept(this)
    val body = node.getInternalBody().accept(this)
    val orelse = node.getInternalOrelse().accept(this)
    return s"$body if $test else $orelse"
  }
  
  override def visitDict(node: Dict): String = {
    // println("visitDict");
    return "{"+implodeStringList((node.getInternalKeys().toList zip node.getInternalValues().toList).map({ case (key, value) => key.accept(this) + ": "+ value.accept(this) }), ",")+"}"
  }
  
  override def visitSet(node: Set): String = {
    // println("visitSet")
    return "{"+implodeList(node.getInternalElts(), ", ")+"}"
  }
  
  override def visitListComp(node: ListComp): String = {
    // println("visitListComp");
    val elt = node.getInternalElt().accept(this)
    val generators = if (!node.getInternalGenerators().isEmpty()) node.getInternalGenerators().toList.foldLeft("")((acc,node) => acc + " " + visitComprehension(node)) else ""

    node.getInternalGenerators().toList.foreach((node) => visitComprehension(node))
    return s"[$elt$generators]"
  }
  
  override def visitSetComp(node: SetComp): String = {
    // println("visitSetComp");
    val elt = node.getInternalElt().accept(this)
    val generators = if (!node.getInternalGenerators().isEmpty()) node.getInternalGenerators().toList.foldLeft("")((acc,node) => acc + " " + visitComprehension(node)) else ""

    node.getInternalGenerators().toList.foreach((node) => visitComprehension(node))
    return s"{$elt$generators}"
  }
  
  override def visitDictComp(node: DictComp): String = {
    // println("visitDictComp");
    val key = node.getInternalKey().accept(this)
    val value = node.getInternalValue().accept(this)
    val generators = if (!node.getInternalGenerators().isEmpty()) node.getInternalGenerators().toList.foldLeft("")((acc,node) => acc + " " + visitComprehension(node)) else ""

    node.getInternalGenerators().toList.foreach((node) => visitComprehension(node))
    return s"{$key: $value$generators}";
  }
  
  override def visitGeneratorExp(node: GeneratorExp): String = {
    // println("visitGeneratorExp");
    val elt = node.getInternalElt().accept(this)
    val generators = if (!node.getInternalGenerators().isEmpty()) node.getInternalGenerators().toList.foldLeft("")((acc,node) => acc + " " + visitComprehension(node)) else ""

    node.getInternalGenerators().toList.foreach((node) => visitComprehension(node))
    return s"($elt$generators)"
  }
  
  override def visitYield(node: Yield): String = {
    // println("visitYield")
    return "yield " + node.getInternalValue().accept(this)
  }
  
  override def visitCompare(node: Compare): String = {
    // println("visitCompare");

    val pairList = node.getInternalComparators().toList zip node.getInternalOps().toList

    return pairList.map({ case (com,op) => cmpopTypeToString(op) + " " + com.accept(this)}).foldLeft(node.getInternalLeft().accept(this))((str,add) => str + " " + add)
  }
  
  override def visitCall(node: Call): String = {
    // println("visitCall")
    
    val func = node.getInternalFunc().accept(this)
    
    val args = implodeList(node.getInternalArgs(), ", ")
    val keywords = implodeStringList(node.getInternalKeywords().toList.map((node) => node.getInternalArg() + "=" + node.getInternalValue().accept(this)), ", ", true)
    val kwargs = if (node.getInternalKwargs() != null) "**" + node.getInternalKwargs().accept(this) else ""
    val starargs = if (node.getInternalStarargs() != null) "*" + node.getInternalStarargs().accept(this) else ""
    val mixed_args = implodeStringList(scala.List(args, kwargs, starargs, keywords), ", ", true)
    
    return s"$func($mixed_args)"
  }
  
  override def visitRepr(node: Repr): String = {
    // println("visitRepr");
    return "`"+node.getInternalValue().accept(this)+"`"
  }
  
  override def visitNum(node: Num): String = {
    // println("visitNum");
    return node.getInternalN().toString();
  }
  
  override def visitStr(node: Str): String = {
    // println("visitStr");
    return "\"" + node.getInternalS().toString().replaceAll("\\\\", "\\\\\\\\")
                                                .replaceAll("\n","\\\\n")
                                                .replaceAll("\t", "\\\\t")
                                                .replaceAll("\r", "\\\\r")
                                                .replaceAll("\b", "\\\\b")
                                                .replaceAll("\f", "\\\\f")
                                                .replaceAll("\"", "\\\\\"")
                                                .replaceAll("\'", "\\\\\'") + "\""; // HACK
  }
  
  override def visitAttribute(node: Attribute): String = {
    // println("visitAttribute");
    return node.getInternalValue().accept(this) + "." + node.getInternalAttrName().accept(this);
  }
  
  override def visitSubscript(node: Subscript): String = {
    // println("visitSubscript");
    val slice = node.getInternalSlice().accept(this)
    val value = node.getInternalValue().accept(this)
    return s"$value[$slice]";
  }
  
  override def visitName(node: Name): String = {
    // println("visitName");
    return node.getInternalId();
  }
  
  override def visitList(node: List): String = {
    // println("visitList");
    val elts = implodeList(node.getInternalElts(), ", ")
    return s"[$elts]";
  }
  
  override def visitTuple(node: Tuple): String = {
    // println("visitTuple");
    val elts = implodeList(node.getInternalElts(), ", ")
    return s"($elts)";
  }
  
  override def visitEllipsis(node: Ellipsis): String = {
    // println("visitEllipsis");
    return "Ellipsis"
  }
  
  override def visitSlice(node: Slice): String = {

    val lower = if (node.getInternalLower() != null) node.getInternalLower().accept(this) else ""
    val upper = if (node.getInternalUpper() != null) node.getInternalUpper().accept(this) else ""
    val step = if (node.getInternalStep() != null) ":"+node.getInternalStep().accept(this) else ""
    return s"$lower:$upper$step";
  }
  
  override def visitExtSlice(node: ExtSlice): String = {
    // println("visitExtSlice");
    return implodeList(node.getInternalDims(), ", ");
  }
  
  override def visitIndex(node: Index): String = {
    // println("visitIndex");
    return node.getInternalValue().accept(this)
  }
  
  override def visitExceptHandler(node: ExceptHandler): String = {
    // println("visitExceptHandler");
    val itype = if (node.getInternalType() != null) node.getInternalType().accept(this) else ""
    val iname = if (node.getInternalName() != null) node.getInternalName().accept(this) else ""
    incIndent()
    val then_branch = implodeList(node.getInternalBody(), "\n")
    decIndent()
    if (itype != "" && iname != "")
      return indent(s"except $itype as $iname:\n$then_branch")
    if (itype != "")
      return indent(s"except $itype:\n$then_branch")
    return indent(s"except:\n$then_branch")
  }
}