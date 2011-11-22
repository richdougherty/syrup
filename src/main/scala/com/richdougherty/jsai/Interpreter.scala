package com.richdougherty.jsai

import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.GenTraversableOnce
import scala.util.continuations._
import Parser.AdditionOperator
import Parser.CallExpression
import Parser.Expression
import Parser.ExpressionStatement
import Parser.FunctionDeclarationSourceElement
import Parser.InfixExpression
import Parser.ReturnStatement
import Parser.SourceElement
import Parser.StatementSourceElement
import Parser.StringLiteral

class Interpreter {
  
  // From https://raw.github.com/JamesEarlDouglas/funtinuations/master/MonadicContinuations.scala
  // implicit coversion from iterable-like collections to cps-friendly collections
  implicit def cpsIterable[A, Repr](xs: IterableLike[A, Repr]) = new {
    def cps = new {
      def foreach[B](f: A => Any @cps[MachineOp]): Unit @cps[MachineOp] = {
        val it = xs.iterator
        while(it.hasNext) f(it.next)
      }
      def map[B, That](f: A => B @cps[MachineOp])(implicit cbf: CanBuildFrom[Repr, B, That]): That @cps[MachineOp] = {
        val b = cbf(xs.repr)
        foreach(b += f(_))
        b.result
      }
      def flatMap[B, That](f: A => GenTraversableOnce[B] @cps[MachineOp])(implicit cbf: CanBuildFrom[Repr, B, That]): That @cps[MachineOp] = {
        val b = cbf(xs.repr)
        for (x <- this) b ++= f(x)
        b.result
      }
      def filter(f: A => Boolean @cps[MachineOp])(implicit cbf: CanBuildFrom[Repr, A, Repr]): Repr @cps[MachineOp] = {
        val b = cbf(xs.repr)
        for (x <- this) 
          if (f(x)) b += x
        b.result
      }
      def foldLeft[B](z: B)(f: (B, A) => B @cps[MachineOp]): B @cps[MachineOp] = {
        val it = xs.iterator
        var acc: B = z
        while(it.hasNext) acc = f(acc, it.next)
        acc
      }
      def reduceLeft[B >: A](f: (B, A) => B @cps[MachineOp]): B @cps[MachineOp] = {
        if (xs.isEmpty)
          throw new UnsupportedOperationException("empty.reduceLeft")
    
        val it = xs.iterator
        var acc: B = it.next
        while(it.hasNext) acc = f(acc, it.next)
        acc
      }
    }
  }

  sealed trait Val
  case object VUndef extends Val
  case object VNull extends Val
  case class VBool(d: Boolean) extends Val
  case class VStr(d: String) extends Val
  case class VNum(d: Double) extends Val
  case class VObj(d: ObjPtr) extends Val
  case class VRef(base: Val, refName: String, strictRef: Boolean) extends Val
  
  sealed trait Typ
  case object TyUndef extends Typ
  case object TyNull extends Typ
  case object TyBool extends Typ
  case object TyStr extends Typ
  case object TyNum extends Typ
  case object TyObj extends Typ
  case object TyRef extends Typ

  type ObjPtr = Int
  case class ObjData(d: Map[PropName, Prop], ipm: InternalPropMap)

  type PropName = String
  sealed trait Prop
  case class NamedDataProp(
    value: Val,
    writable: Val,
    enumerable: Val,
    configurable: Val) extends Prop
  case class NamedAccessorProp(
    get: Val,
    set: Val,
    enumerable: Val,
    configurable: Val) extends Prop

  trait InternalProp
  case class IPPrototype(v: Val) extends InternalProp
  case class IPClass(v: Val) extends InternalProp
  case class IPExtensible(v: Val) extends InternalProp
  case class IPCall(meth: (Val, Val) => Val) extends InternalProp

  case class InternalPropMap(d: List[InternalProp]) {
    def contains[P <: InternalProp]: Boolean = {
      d.exists(_.isInstanceOf[P])
    }
    def get[P <: InternalProp]: Option[P] = {
      d.find(_.isInstanceOf[P]).asInstanceOf[Option[P]]
    }
    def put[P <: InternalProp](ip: P): InternalPropMap = {
      InternalPropMap(ip :: (d.filter(!_.isInstanceOf[P])))
    }
  }

  case class Machine(cxt: ExecutionContext, heap: Heap, globalObjPtr: ObjPtr, globalEnv: LexicalEnvironment)
  case class ExecutionContext(varEnv: LexicalEnvironment, lexEnv: LexicalEnvironment, thisBinding: ObjPtr)
  case class Heap(mem: Map[ObjPtr, ObjData], nextPtr: ObjPtr)

  case class LexicalEnvironment(er: EnvironmentRecord, outer: Option[EnvironmentRecord])

  sealed trait EnvironmentRecord
  case class DeclarativeEnvironmentRecord(bindings: Map[String, Binding]) extends EnvironmentRecord
  case class ObjectEnvironmentRecord(bindingObj: ObjPtr, provideThis: Boolean = false) extends EnvironmentRecord

  sealed trait Binding
  case class MutableBinding(v: Option[Val], canDelete: Boolean) extends Binding
  case class ImmutableBinding(v: Option[Val]) extends Binding

  case class Completion(typ: CompletionType, v: Option[Val], target: Option[String])
  sealed trait CompletionType
  case object CNormal extends CompletionType
  case object CBreak extends CompletionType
  case object CContinue extends CompletionType
  case object CReturn extends CompletionType
  case object CThrow extends CompletionType
  
  sealed trait MachineOp
  case class MOAccess(thunk: Machine => MachineOp) extends MachineOp
  case class MOUpdate(thunk: Machine => (Machine, MachineOp)) extends MachineOp
  case class MOComp(m: Machine, c: Completion) extends MachineOp
  
  def runMachineOp(m: Machine, mo: MachineOp): (Machine, Completion) = mo match {
    case MOAccess(thunk) => {
      val mo1 = thunk(m)
      runMachineOp(m, mo1)
    }
    case MOUpdate(thunk) => {
      val (m1, mo1) = thunk(m)
      runMachineOp(m1, mo1)
    }
    case MOComp(m, c) => (m, c)
  }
  
  def moAccess[A](access: ((Machine, (A => MachineOp)) => MachineOp)): A @cps[MachineOp] = {
    shift((k: A => MachineOp) => MOAccess((m: Machine) => access(m, k)))
  }

  def moUpdate[A](update: ((Machine, (A => MachineOp)) => (Machine, MachineOp))): A @cps[MachineOp] = {
    shift((k: A => MachineOp) => MOUpdate((m: Machine) => update(m, k)))
  }

  def newPtr = moUpdate[ObjPtr] {(m: Machine, k: ObjPtr => MachineOp) =>
    val h = m.heap
    val ptr = h.nextPtr
    val m1 = m.copy(heap = h.copy(nextPtr = ptr+1))
    (m1, k(ptr))
  }

  def load(ptr: ObjPtr) = moAccess[ObjData] {(m: Machine, k: ObjData => MachineOp) =>
    k(m.heap.mem(ptr)) // FIXME: Handle missing object.
  }

  def store(ptr: ObjPtr, data: ObjData) = moUpdate[Unit] {(m: Machine, k: Unit => MachineOp) =>
    (m.copy(heap = m.heap.copy(mem = m.heap.mem.updated(ptr, data))), k(()))
  }

  def getDirectivePrologue(ses: List[SourceElement]): List[String] = {
    // FIXME: Use quoted, unescaped string literals as required by spec.
    def seToSl(se: SourceElement): Option[String] = se match {
      case StatementSourceElement(st) => st match {
        case ExpressionStatement(ex) => ex match {
          case StringLiteral(d) => Some(d)
          case _ => None
        }
        case _ => None
      }
      case _ => None
    }
    ses.takeWhile(seToSl(_).isDefined).map(seToSl(_).get)
  }

  def hasUseStrictDirective(ses: List[SourceElement]): Boolean = {
    getDirectivePrologue(ses).exists(_ == "use strict")
  }

  // Type(V) in spec
  def typ(v: Val) = v match {
    case VUndef => TyUndef
    case VNull => TyNull
    case VBool(_) => TyBool
    case VStr(_) => TyStr
    case VNum(_) => TyNum
    case VObj(_) => TyObj
    case VRef(_, _, _) => TyRef
  }
  
  // GetValue(V) in spec
  def getValue(v: Val): Val = {
    v // FIXME: Stub
  }

  // ToPrimitive(V) in spec
  def toPrimitive(v: Val): Val = {
    v // FIXME: Stub
  }

  // ToString(V) in spec
  def toString(v: Val): VStr = {
    v.asInstanceOf[VStr] // FIXME: Stub
  }

  // ToNumber(V) in spec
  def toNumber(v: Val): VNum = {
    v.asInstanceOf[VNum] // FIXME: Stub
  }
  
  def isCallable(v: Val): Boolean @cps[MachineOp] = v match {
    case VObj(objPtr) => {
      val objData = load(objPtr)
      objData.ipm.contains[IPCall]
    }
    case _ => false
  }

  def evaluateExpression(expr: Expression): Val @cps[MachineOp] = expr match {
    case InfixExpression(l, op, r) => op match {
      case AdditionOperator => {
        val lref = evaluateExpression(l)
        val lval = getValue(lref)
        val rref = evaluateExpression(r)
        val rval = getValue(rref)
        val lprim = toPrimitive(lval)
        val rprim = toPrimitive(rval)
        (lprim, rprim) match {
          case (VStr(_), _) | (_, VStr(_)) => {
            val lstr = toString(lprim)
            val rstr = toString(rprim)
            VStr(lstr.d + rstr.d)
          }
          case _ => {
            val lnum = toNumber(lprim)
            val rnum = toNumber(rprim)
            VNum(lnum.d + rnum.d)
          }
        }
      }
    }
    case StringLiteral(s) => VStr(s)
    case CallExpression(target, args) => {
//    Let ref be the result of evaluating MemberExpression.
      val ref = evaluateExpression(target)
//    Let func be GetValue(ref).
      val func = getValue(ref)
//    Let argList be the result of evaluating Arguments, producing an internal list of argument values (see 11.2.4).
      val argList = for (argExpr <- args.cps) yield {
        val ref = evaluateExpression(argExpr)
        val arg = getValue(ref)
        arg
      }
//    If Type(func) is not Object, throw a TypeError exception.
      assert(typ(func) == TyObj) // FIXME: Should be a TypeError
//    If IsCallable(func) is false, throw a TypeError exception.
      assert(isCallable(func))
//    If Type(ref) is Reference, then
//        If IsPropertyReference(ref) is true, then
//            Let thisValue be GetBase(ref).
//        Else, the base of ref is an Environment Record
//            Let thisValue be the result of calling the ImplicitThisValue concrete method of GetBase(ref).
//    Else, Type(ref) is not Reference.
//        Let thisValue be undefined.
//    Return the result of calling the [[Call]] internal method on func, providing thisValue as the this value and providing the list argList as the argument values.
//The production CallExpression : CallExpression Arguments is evaluated in exactly the same manner, except that the contained CallExpression is evaluated in step 1.
      VUndef // FIXME: Stub
    }
  }

  def evaluateSourceElement(se: SourceElement): Completion @cps[MachineOp] = se match {
    case StatementSourceElement(st) => st match {
      case ExpressionStatement(expr) => {
        val exprRef = evaluateExpression(expr)
        Completion(CNormal, Some(getValue(exprRef)), None)
      }
      case ReturnStatement(expr) => expr match {
        case None => Completion(CReturn, None, None)
        case Some(expr0) => {
          val exprRef = evaluateExpression(expr0)
          Completion(CReturn, Some(getValue(exprRef)), None)
        }
      }
    }
    case FunctionDeclarationSourceElement(_) =>
      Completion(CNormal, None, None)
  }

  def evaluateSourceElements(ses: List[SourceElement]): Completion @cps[MachineOp] = {
    ses.cps.foldLeft(Completion(CNormal, None, None)) {
      case (Completion(CNormal, prevVal, prevTarget), se) => {
        val currCompletion = evaluateSourceElement(se)
        if (currCompletion.v.isDefined) currCompletion else currCompletion.copy(v = prevVal)
      }
      case (abrupt, _) => abrupt
    }
  }
  
  sealed trait Code
  case class GlobalCode(ses: List[SourceElement]) extends Code
  case class EvalCode(ses: List[SourceElement], directStrictCall: Boolean) extends Code
  case class FunctionCode(func: ObjPtr, ses: List[SourceElement], declaredInStrict: Boolean) extends Code
  
  def isStrictModeCode(code: Code): Boolean = code match {
    case GlobalCode(ses) => hasUseStrictDirective(ses)
    case EvalCode(ses, directStrictCall) => hasUseStrictDirective(ses) || directStrictCall
    case FunctionCode(_, ses, declaredInStrict) => declaredInStrict || hasUseStrictDirective(ses)
  }
  
  def instantiateDeclarationBinding(m: Machine, code: Code, args: List[Val]): Machine = {
    val env = m.cxt.varEnv
    val configurableBindings = code match {
      case _: EvalCode => true
      case _ => false
    }
    val strict = isStrictModeCode(code)
//    code match {
//      case FunctionCode(func, _, _) => {
//        val names = Nil.asInstanceOf[List[String]] //getInternalProperty(func, "[[FormalParameters]]")
//        val argCount = args.length
//        for ((argName, n) <- names.zipWithIndex) {
//          val v = if (n > argCount) VUndef else args(n)
//          val argAlreadyDeclared = hasBinding(env, argName)
//          if (!argAlreadyDeclared) createMutableBinding(env, argName)
//          setMutableBinding(env, v, strict)
//        }
//      }
//      case _ => ()
//    }
    m // FIXME: Stub
  }

  def interpret(programSource: String): Completion = {
    val p = Parser.parse(programSource)
    val globalPtr = 1 // Stubbed ObjPtr to global object
    val globalEnv = LexicalEnvironment(ObjectEnvironmentRecord(globalPtr), None)
    val strictMode = hasUseStrictDirective(p.ses)
    if (p.ses.isEmpty) return Completion(CNormal, None, None) // Optimization from spec
    val progCxt = ExecutionContext(globalEnv, globalEnv, globalPtr)
    val m = Machine(progCxt, Heap(Map.empty, 0), globalPtr, globalEnv)
    runMachineOp(m, reset {
      val result = evaluateSourceElements(p.ses)
      MOComp(m, result)
    })._2
  }

}