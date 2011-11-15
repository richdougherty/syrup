package com.richdougherty.jsai

import org.mozilla.javascript.CompilerEnvirons
import org.mozilla.javascript.Context
import org.mozilla.javascript.DefaultErrorReporter
import org.mozilla.javascript.Parser
import org.mozilla.javascript.Scriptable

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

      val parser = new Parser()
      val astRoot = parser.parse(source, sourceFileName, sourceLineNo)

      val scope = cx.initStandardObjects()
      val result = cx.evaluateString(scope, source, sourceFileName, sourceLineNo, null)
      println(result)
    } finally {
      Context.exit()
    }
  }

}
