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
import org.mozilla.javascript.InterpreterData

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
      cx.setOptimizationLevel(-1);

      val source = "'Hello '+'world.'"
      val sourceFileName = "HelloWorld"
      val sourceLineNo = 1

      val scope = cx.initStandardObjects()
      val script = cx.compileString(source, sourceFileName, sourceLineNo, null)
      val result = script.exec(cx, scope)
      println(result)
    } finally {
      Context.exit()
    }
  }

}
