package com.richdougherty.jsai

import scala.util.continuations._

object ContinuationsTest {

  def stop: Nothing @cps[Unit] = shift((k: Nothing => Unit) => ())
  
  def dummy: Unit @cps[Unit] = shift((k: Unit => Unit) => k(()))
  
  def test1(x: Int): Unit @cps[Unit] = {
    if (x > 1) stop
    else dummy // Fails if this line is commented out
    ()
  }

  def test2(f: () => Option[Int] @cps[Unit]): Unit @cps[Unit] = {
    val v = f.apply
    v match {
      case None => ()
      case Some(i) => ()
    }
    // Fails unless store result before match
    //f.apply match {
    //  case None => ()
    //  case Some(i) => ()
    //}
  }

  def main(args: Array[String]) {
    reset(test3(None))
  }

  def test3(v: Option[Int]): Unit @cps[Unit] = {
    dummy
    v match {
      // Compiler warning: expression () of type Unit @scala.util.continuations.cpsPlus @util.continuations.package.cps[Unit] is not expected to have a cps type
      // Runtime exception: scala.runtime.BoxedUnit cannot be cast to scala.util.continuations.ControlContext
      case None => ()
      case Some(i) => {
        dummy
        ()
      }
    }
  }

  def test4(f: () => Int @cps[Unit]): Unit @cps[Unit] = {
    //f() // Fails if uncommented: should be able to convert types to Unit
  }

  // XXX: Test early returns

  // XXX: Test inherit @cps annotation when return type omitted
  // trait X { def a: Unit @cps[Unit] }
  // class Y extends X { def a = () } 

}