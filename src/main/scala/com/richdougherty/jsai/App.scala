package com.richdougherty.jsai

import org.mozilla.javascript.CompilerEnvirons
import org.mozilla.javascript.Context
import org.mozilla.javascript.DefaultErrorReporter
import org.mozilla.javascript.Parser
import org.mozilla.javascript.Scriptable
import org.mozilla.javascript.IRFactory
import org.mozilla.javascript.ErrorReporter
import org.mozilla.javascript.EvaluatorException
import org.mozilla.javascript.Interpreter

/**
 * @author Rich Dougherty <http://www.richdougherty.com/>
 */
object App {
  
  def main(args : Array[String]) {
    // See:
    //   Embedding JavaScript in Java with Rhino
    //   http://www.informit.com/guides/content.aspx?g=java&seqNum=562
    val cx = Context.enter()
    try {
      val source = "'Hello '+'world.'"
      val sourceFileName = "HelloWorld"
      val sourceLineNo = 1

      val compilerEnv = new CompilerEnvirons()
      val errorReporter = new ErrorReporter {
	    def warning(message: String, sourceName: String, line: Int,
	                 lineSource: String, lineOffset: Int) = {
	      // TODO: Do something?
	    }
	    def error(message: String, sourceName: String, line: Int,
	               lineSource: String, lineOffset: Int) = {
	      throw runtimeError(
                message, sourceName, line, lineSource, lineOffset);
	    }
	    def runtimeError(message: String, sourceName: String,
	                                    line: Int, lineSource: String,
	                                    lineOffset: Int): EvaluatorException = {
	      new EvaluatorException(
                message, sourceName, line, lineSource, lineOffset)
	    }
      }
      val parser = new Parser(compilerEnv, errorReporter)
      val ast = parser.parse(source, sourceFileName, sourceLineNo)
      val irf = new IRFactory(compilerEnv, errorReporter)
      var tree = irf.transformTree(ast)
      val compiler = new Interpreter()
      val bytecode = compiler.compile(compilerEnv,
                                           tree, tree.getEncodedSource(),
                                           false)
      val script = compiler.createScriptObject(bytecode, null)
      println(script)

      val scope = cx.initStandardObjects()
      val result = cx.evaluateString(scope, source, sourceFileName, sourceLineNo, null)
      println(result)
    } finally {
      Context.exit()
    }
  }

}
