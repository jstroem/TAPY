package tapy.typeanalysis

import tapy.constants.StackConstants
import java.lang.ArithmeticException
import org.python.antlr.ast.arguments
import org.python.antlr.ast.Name
import tapy.dfa._
import tapy.dfa.MonotoneFrameworkTypes._
import tapy.cfg._
import tapy.lattices._
import tapy.exceptions._
import tapy.constants
import scala.collection.JavaConversions._

trait Logger {
  val DEBUG = true
  var last = ""
  
  def log(node: String, msg: String): Unit =
    if (DEBUG) {
      val str = s"[$node] $msg"
      if (str != last) {
        // println(s"[$node] $msg")
        last = str
      }
    }
}