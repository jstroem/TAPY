import _root_.org.python.antlr._;

object Main {
  var node: _root_.org.python.antlr.base.mod = null
  
  def main(args: Array[String]): Unit = {
    val parser = new BaseParser(null, args(0), "ascii");
    node = parser.parseModule()
    println(node);
  }
}