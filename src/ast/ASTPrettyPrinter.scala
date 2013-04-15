package tapy.cfg

import org.python.antlr._
import org.python.antlr.ast._;
import sun.security.util.Length
import scala.collection.JavaConversions._

object ASTPrettyPrinter extends VisitorBase[String] {
  def implodeList(lst: java.util.List[_ <: PythonTree], sep:String =  "") : String = {
    val list = lst.toList
    return list.headOption match {
      case Some(e) => list.tail.foldLeft(e.accept(this))((list,el) => list + sep + el.accept(this))
      case None => ""
    }
  }

  def CmpopTypeToString(cmpOp: cmpopType) : String = cmpOp match {
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

  var indent : String = ""

  override def traverse(node: PythonTree): Unit = {
      node.traverse(this);
  }
  
  override def unhandled_node(node: PythonTree): String = {
      return "<not implemented>";
  }

  /* Implementation of visitor methods: */
  
  override def visitModule(node: Module): String = {
    var result: String = "";
    return implodeList(node.getInternalBody(), "\n")
  }
  
  override def visitInteractive(node: Interactive): String = {
    println("visitInteractive");
    return "<not implemented>";
  }
  
  override def visitExpression(node: Expression): String = {
    println("visitExpression");
    return "<not implemented>";
  }
  
  override def visitSuite(node: Suite): String = {
    println("visitSuite");
    return "<not implemented>";
  }
  
  override def visitFunctionDef(node: FunctionDef): String = {
    println("visitFunctionDef");
    return "<not implemented>";
  }
  
  override def visitClassDef(node: ClassDef): String = {
    println("visitClassDef");
    val prevIndent = indent
    indent += "  "

    val res = "class " + node.getInternalName() + "(" + implodeList(node.getInternalBases(),", ") + "):\n" + 
        implodeList(node.getInternalBody(), "\n")

    indent = prevIndent

    return res
  }
  
  override def visitReturn(node: Return): String = {
    println("visitReturn");
    return indent + "return" + node.getInternalValue().accept(this);
  }
  
  override def visitDelete(node: Delete): String = {
    println("visitDelete");
    return "<not implemented>";
  }
  
  override def visitAssign(node: Assign): String = {
    println("visitAssign")
    return indent + implodeList(node.getInternalTargets(), ", ") + " = " + node.getInternalValue().accept(this)
  }
  
  override def visitAugAssign(node: AugAssign): String = {
    println("visitAugAssign");
    return "<not implemented>";
  }
  
  override def visitPrint(node: Print): String = {
    println("visitPrint");
    return "<not implemented>";
  }
  
  override def visitFor(node: For): String = {
    println("visitFor");
    return "<not implemented>";
  }
  
  override def visitWhile(node: While): String = {
    println("visitWhile");
    return "<not implemented>";
  }
  
  override def visitIf(node: If): String = {
    var res = indent + "if " + node.getInternalTest().accept(this) + ":\n"

    val prevIndent = indent
    indent += "  "

    res += implodeList(node.getInternalBody(), "\n")

    if (node.getInternalOrelse() != null) {
      res += "\n" + prevIndent + "else:\n" + implodeList(node.getInternalOrelse(), "\n")
    }

    indent = prevIndent
    return res;
  }
  
  override def visitWith(node: With): String = {
    println("visitWith");
    return "<not implemented>";
  }
  
  override def visitRaise(node: Raise): String = {
    println("visitRaise");
    return "<not implemented>";
  }
  
  override def visitTryExcept(node: TryExcept): String = {
    println("visitTryExcept");
    return "<not implemented>";
  }
  
  override def visitTryFinally(node: TryFinally): String = {
    println("visitTryFinally");
    return "<not implemented>";
  }
  
  override def visitAssert(node: Assert): String = {
    println("visitAssert");
    return "<not implemented>";
  }
  
  override def visitImport(node: Import): String = {
    println("visitImport");
    return "<not implemented>";
  }
  
  override def visitImportFrom(node: ImportFrom): String = {
    println("visitImportFrom");
    return "<not implemented>";
  }
  
  override def visitExec(node: Exec): String = {
    println("visitExec");
    return "<not implemented>";
  }
  
  override def visitGlobal(node: Global): String = {
    println("visitGlobal");
    return "<not implemented>";
  }
  
  override def visitExpr(node: Expr): String = {
    println("visitExpr");
    return node.getInternalValue().accept(this);
  }
  
  override def visitPass(node: Pass): String = {
    println("visitPass");
    return "<not implemented>";
  }
  
  override def visitBreak(node: Break): String = {
    println("visitBreak");
    return "<not implemented>";
  }
  
  override def visitContinue(node: Continue): String = {
    println("visitContinue");
    return "<not implemented>";
  }
  
  override def visitBoolOp(node: BoolOp): String = {
    println("visitBoolOp");
    return "<not implemented>";
  }
  
  override def visitBinOp(node: BinOp): String = {
    println("visitBinOp");
    return "<not implemented>";
  }
  
  override def visitUnaryOp(node: UnaryOp): String = {
    println("visitUnaryOp");
    return "<not implemented>";
  }
  
  override def visitLambda(node: Lambda): String = {
    println("visitLambda");
    return "<not implemented>";
  }
  
  override def visitIfExp(node: IfExp): String = {
    println("visitIfExp")
    return "<not implemented>"
  }
  
  override def visitDict(node: Dict): String = {
    println("visitDict");
    return "<not implemented>";
  }
  
  override def visitSet(node: Set): String = {
    println("visitSet");
    return "<not implemented>";
  }
  
  override def visitListComp(node: ListComp): String = {
    println("visitListComp");
    return "<not implemented>";
  }
  
  override def visitSetComp(node: SetComp): String = {
    println("visitSetComp");
    return "<not implemented>";
  }
  
  override def visitDictComp(node: DictComp): String = {
    println("visitDictComp");
    return "<not implemented>";
  }
  
  override def visitGeneratorExp(node: GeneratorExp): String = {
    println("visitGeneratorExp");
    return "<not implemented>";
  }
  
  override def visitYield(node: Yield): String = {
    println("visitYield");
    return "<not implemented>";
  }
  
  override def visitCompare(node: Compare): String = {
    println("visitCompare");
    println("getInternalComparators: " + implodeList(node.getInternalComparators(), ", "))
    println("getInternalOps: " + node.getInternalOps().toList.map(CmpopTypeToString))

    return "<not implemented>";
  }
  
  override def visitCall(node: Call): String = {
    println("visitCall");
    
    // func
    val func: String = node.getInternalFunc().accept(this);
    println("- " + func);
    
    // args
    var args: String = implodeList(node.getInternalArgs(), ",")
    println("- args: " + args);
    
    // keywords
    var keywords: String = implodeList(node.getInternalKeywords(), ",")
    println("- keywords: " + keywords);
    
    // kwargs
    var kwargs = "";
    if (node.getInternalKwargs() != null) {
      kwargs = "**" + node.getInternalKwargs().accept(this);
    }
    println("- kwargs: " + kwargs);
    
    // starargs
    var starargs = "";
    if (node.getInternalStarargs() != null) {
      starargs = "*" + node.getInternalStarargs().accept(this);
    }
    println("- starargs: " + starargs);
    
    return func + "(" + args + starargs + kwargs + ")";
  }
  
  override def visitRepr(node: Repr): String = {
    println("visitRepr");
    return "<not implemented>";
  }
  
  override def visitNum(node: Num): String = {
    println("visitNum");
    return node.getInternalN().toString();
  }
  
  override def visitStr(node: Str): String = {
    println("visitStr");
    return "\"" + node.getInternalS().toString() + "\""; // HACK
  }
  
  override def visitAttribute(node: Attribute): String = {
    println("visitAttribute");
    return node.getInternalValue().accept(this) + "." + node.getInternalAttrName().accept(this);
  }
  
  override def visitSubscript(node: Subscript): String = {
    println("visitSubscript");
    return "<not implemented>";
  }
  
  override def visitName(node: Name): String = {
    println("visitName");
    return node.getInternalId();
  }
  
  override def visitList(node: List): String = {
    println("visitList");
    return "<not implemented>";
  }
  
  override def visitTuple(node: Tuple): String = {
    println("visitTuple");
    return "<not implemented>";
  }
  
  override def visitEllipsis(node: Ellipsis): String = {
    println("visitEllipsis");
    return "<not implemented>";
  }
  
  override def visitSlice(node: Slice): String = {
    println("visitSlice");
    return "<not implemented>";
  }
  
  override def visitExtSlice(node: ExtSlice): String = {
    println("visitExtSlice");
    return "<not implemented>";
  }
  
  override def visitIndex(node: Index): String = {
    println("visitIndex");
    return "<not implemented>";
  }
  
  override def visitExceptHandler(node: ExceptHandler): String = {
    println("visitExceptHandler");
    return "<not implemented>";
  }
}