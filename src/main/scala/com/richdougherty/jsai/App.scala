package com.richdougherty.jsai

import org.mozilla.javascript.Context
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
      val scope = cx.initStandardObjects()
      val code = "'Hello '+'world.'"
      val result = cx.evaluateString(scope, code, "HelloWorld", 1, null)
      println(result)
    } finally {
      Context.exit()
    }
  }

}
