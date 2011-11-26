package com.richdougherty.jsai

import org.mozilla.javascript.Context

/**
 * @author Rich Dougherty <http://www.richdougherty.com/>
 */
object App {
  
  def main(args : Array[String]) {
    val source = """
      function hello(name) {
        return 'Hello '+name+'.'
      }
      hello('world');
      """

    // See:
    //   Embedding JavaScript in Java with Rhino
    //   http://www.informit.com/guides/content.aspx?g=java&seqNum=562
    val cx = Context.enter()
    try {
      cx.setOptimizationLevel(-1);

      val sourceFileName = "HelloWorld"
      val sourceLineNo = 1

      val scope = cx.initStandardObjects()
      val script = cx.compileString(source, sourceFileName, sourceLineNo, null)
      val result = script.exec(cx, scope)
      println(result)
    } finally {
      Context.exit()
    }
    
    {
      val interp = new Interpreter
      val result = interp.interpret(source)
      println(result)
    }
  }

}
