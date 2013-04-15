package tapy

import _root_.org.python.antlr._;
import _root_.org.python.antlr.base._;
import _root_.org.python.antlr.runtime._;
import _root_.org.python.antlr.runtime.tree._;
import tapy.cfg._

object Main {
	var node: mod = null
	var cfg: ControlFlowGraph = null

	def main(args: Array[String]): Unit = {

		args.foreach((f) => {
			var file = new File(f)
			if (file.isFile()){
				analyzeFile(file, opts)
			} else if (file.isDirectory()) {	
				analyzeDirectory(file, opts)
			} else {
				System.err.println("Couldn't open: " + f)
			}
		})

		val parser: BaseParser = new BaseParser(new ANTLRFileStream("tests/greet.py"), "tests/greet.py", "ascii");
		node = parser.parseModule()

		cfg = node.accept(CFGGeneratorVisitor);

		println(node.toStringTree());
	}

	def analyze(file: File) : Unit = {
		(dir,fname,fext) = splitFilename(file)

		val parser: BaseParser = new BaseParser(new ANTLRFileStream(file.getPath()), file.getPath(), "ascii");
		node = parser.parseModule()
		cfg = node.accept(CFGGeneratorVisitor);
		println(node.toStringTree());
	}

	def analyzeDirectory(dir : File) : Unit = {
		dir.listFiles().foreach((file) => {
			if (file.isFile()){
				analyze(file, opts)
			} else if (file.isDirectory()) {	
				analyzeDirectory(file, opts)
			}
		})
	}

	def splitFilename(f : File) : (String,String,String) = {
		var dir = f.getParent() + "/"
		var file = f.getName()
		var idx = file.lastIndexOf('.')
		var extension = if (idx >= 0)  file.substring(idx) else ""
		var filename = if (idx >= 0) file.substring(0, idx) else file
		(dir,filename,extension)
	}

}