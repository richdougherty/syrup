package com.richdougherty.jsai

import org.mozilla.javascript.Context

/**
 * @author Rich Dougherty <http://www.richdougherty.com/>
 */
object App {
  
  def main(args : Array[String]) {
    test("'Hello world.';")
    test("""
      function hello() {
        return 'Hello world.'
      }
      hello();
    """)
    test("""
      function hello(name) {
        return 'Hello '+name+'.'
      }
      hello('world');
    """)
    test("123;")
    test("""
      var x = 123;
      x;
    """)
    test("""
      var x = 123;
      x = x + 1;
      x += 1;
      x++;
      x
    """)
    test("1 < 2;");
    test("""
      loop: for (var i = 0; i < 10; i++) {}
      i;
    """)
    test("true ? 1 : 0;")
    test("""
      var x = -1;
      if (true) { x = 1; } else { x = 0; }
      x;
    """)
    test("undefined");
    test("var x = {x: 1, y: 2}; x['y']++; x.y;");
    test("var x = [1]; x;");
  }

  def test(source: String): Unit = {
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
