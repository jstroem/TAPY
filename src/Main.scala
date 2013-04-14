package TAPY

import _root_.org.python.antlr._;
import _root_.org.python.antlr.base._;
import _root_.org.python.antlr.runtime._;
import _root_.org.python.antlr.runtime.tree._;


object Main {
  var node: mod = null
  
  def main(args: Array[String]): Unit = {
    
    val parser: BaseParser = new BaseParser(new ANTLRFileStream("tests/greet.py"), "tests/greet.py", "ascii");
    node = parser.parseModule()
    println(node.toStringTree());
  }
}