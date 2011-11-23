package com.richdougherty.jsai

import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.GenTraversableOnce
import scala.util.continuations._
import Parser.AdditionOperator
import Parser.CallExpression
import Parser.Expression
import Parser.ExpressionStatement
import Parser.Identifier
import Parser.FunctionDeclarationSourceElement
import Parser.InfixExpression
import Parser.ReturnStatement
import Parser.SourceElement
import Parser.StatementSourceElement
import Parser.StringLiteral
import com.richdougherty.jsai.Parser.FunctionDeclaration

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

  sealed trait LangVal extends Val
  case object VUndef extends LangVal
  case object VNull extends LangVal
  case class VBool(d: Boolean) extends LangVal
  case class VStr(d: String) extends LangVal
  case class VNum(d: Double) extends LangVal
  case class VObj(d: ObjPtr) extends LangVal

  sealed trait SpecVal extends Val
  case class VRef(base: Val, refName: String, strictRef: Boolean) extends Val
  
  sealed trait Typ
  case object TyUndef extends Typ
  case object TyNull extends Typ
  case object TyBool extends Typ
  case object TyStr extends Typ
  case object TyNum extends Typ
  case object TyObj extends Typ
  case object TyRef extends Typ
  case object TyEnvRec extends Typ

  type ObjPtr = Int
  trait ObjData {
    // Required methods
    def prototype: Option[VObj]
    def clazz: Val
    def extensible: Val
    def get(propertyName: String): LangVal
    def getOwnProperty(propertyName: String): Option[Prop]
    def getProperty(propertyName: String): Option[Prop] @cps[MachineOp]
    def put(propertyName: String, v: Val, strict: Boolean): Val
    def canPut(propertyName: String): Boolean
    def hasProperty(propertyName: String): Boolean @cps[MachineOp]
    def delete(propertyName: String, failureHandling: Boolean): Boolean
    def defaultValue(hint: String): Val // returns primitive vals only
    def defineOwnProperty(propertyName: String, propDesc: Prop, failureHandling: Boolean): Boolean
    // Optional methods
    protected def ??? = error("Not implemented")
    def primitiveVal: LangVal = ???
    def construct(args: List[Val]): VObj = ???
    def hasInstance(obj: Val): Boolean = ???
    def scope: LexicalEnvironment = ???
    def formalParameters: List[String] = ???
    def code: FunctionCode = ???
    def targetFunction: VObj = ???
    def boundThis: VObj = ???
    def boundArguments: List[Val] = ???
    def `match`(s: String, index: Int): VObj = ???
    def parameterMap: VObj = ???
  }
  trait CallableObj {
    def call(thisObj: Val, args: List[Val]): Val
  }
  case class NativeObj(d: Map[PropName, Prop] = Map.empty, prototype: Option[VObj] = None) extends ObjData {
    def clazz: Val = ???
    def extensible: Val = ???
    def get(propertyName: String): LangVal = ???
    def getOwnProperty(propertyName: String): Option[Prop] = {
      d.get(propertyName) // Spec returns a copy of the (mutable) property descriptor
    }
    def getProperty(propertyName: String): Option[Prop] @cps[MachineOp] = {
      val propOption = getOwnProperty(propertyName)
      propOption match {
        case Some(_) => propOption
        case None => {
          val protoOption = prototype
          protoOption match {
            case None => None
            case Some(proto) => {
              val protoData = load(proto)
              protoData.getProperty(propertyName)
            }
          }
        }
      }
    }
    def put(propertyName: String, v: Val, strict: Boolean): Val = ???
    def canPut(propertyName: String): Boolean = ???
    def hasProperty(propertyName: String): Boolean @cps[MachineOp] = {
      val desc = getProperty(propertyName)
      desc.isDefined
    }
    def delete(propertyName: String, failureHandling: Boolean): Boolean = ???
    def defaultValue(hint: String): Val = ???
    def defineOwnProperty(propertyName: String, propDesc: Prop, failureHandling: Boolean): Boolean = ???
  }

  type PropName = String
  sealed trait Prop
  case class DataProp(
    value: Val,
    writable: Val,
    enumerable: Val,
    configurable: Val) extends Prop
  case class AccessorProp(
    get: Val,
    set: Val,
    enumerable: Val,
    configurable: Val) extends Prop

  case class Machine(cxt: ExecutionContext, heap: Heap, globalObj: VObj, globalEnv: LexicalEnvironment)
  case class ExecutionContext(varEnv: LexicalEnvironment, lexEnv: LexicalEnvironment, thisBinding: VObj, code: Code)
  case class Heap(mem: Map[ObjPtr, ObjData], nextPtr: ObjPtr)

  case class LexicalEnvironment(er: EnvironmentRecord, outer: Option[LexicalEnvironment])

  sealed trait EnvironmentRecord extends SpecVal {
    def hasBinding(name: String): Boolean @cps[MachineOp]
    def implicitThisValue: Val
  }
  case class DeclarativeEnvironmentRecord(bindings: Map[String, Binding]) extends EnvironmentRecord {
    def hasBinding(name: String): Boolean @cps[MachineOp] = bindings.contains(name)
    def implicitThisValue: Val = VUndef
  }
  case class ObjectEnvironmentRecord(bindingObj: VObj, provideThis: Boolean = false) extends EnvironmentRecord {
    def hasBinding(name: String): Boolean @cps[MachineOp] = {
      val bindingObjData = load(bindingObj)
      bindingObjData.hasProperty(name)
    }
    def implicitThisValue: Val = {
      if (provideThis) bindingObj else VUndef
    }
  }

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
  case class MOComp(thunk: Machine => Completion) extends MachineOp
  
  def runMachineOp(m: Machine, mo: MachineOp): (Machine, Completion) = mo match {
    case MOAccess(thunk) => {
      val mo1 = thunk(m)
      runMachineOp(m, mo1)
    }
    case MOUpdate(thunk) => {
      val (m1, mo1) = thunk(m)
      runMachineOp(m1, mo1)
    }
    case MOComp(thunk) => (m, thunk(m))
  }
  
  def moAccess[A](access: ((Machine, (A => MachineOp)) => MachineOp)): A @cps[MachineOp] = {
    shift((k: A => MachineOp) => MOAccess((m: Machine) => access(m, k)))
  }

  def moUpdate[A](update: ((Machine, (A => MachineOp)) => (Machine, MachineOp))): A @cps[MachineOp] = {
    shift((k: A => MachineOp) => MOUpdate((m: Machine) => update(m, k)))
  }

  def moComplete(c: Completion): Nothing @cps[MachineOp] = shift((k: Nothing => MachineOp) => {
    MOComp((_: Machine) => c) // FIXME: Look up actual value.
  })

  def moThrow(errorType: String): Nothing @cps[MachineOp] = {
    // FIXME: Create a proper error object
    val error = VStr(errorType + ": " + (new RuntimeException(errorType)).getStackTraceString)
    moComplete(Completion(CThrow, Some(error), None))
  }

  // Pass a value; used to work around continuations plugin bugs.
  def moVal[A](a: A) = moAccess[A] { (m: Machine, k: A => MachineOp) =>
    k(a) // FIXME: Handle missing object.
  }

  // Does nothing; used to work around continuations plugin bugs.
  def moNop = moVal(())

  def newObj = moUpdate[VObj] {(m: Machine, k: VObj => MachineOp) =>
    val h = m.heap
    val ptr = h.nextPtr
    val m1 = m.copy(heap = h.copy(nextPtr = ptr+1))
    (m1, k(VObj(ptr)))
  }

  def load(obj: VObj) = moAccess[ObjData] {(m: Machine, k: ObjData => MachineOp) =>
    k(m.heap.mem(obj.d)) // FIXME: Handle missing object.
  }

  def store(ptr: ObjPtr, data: ObjData) = moUpdate[Unit] {(m: Machine, k: Unit => MachineOp) =>
    (m.copy(heap = m.heap.copy(mem = m.heap.mem.updated(ptr, data))), k(()))
  }

  def currentCxt = moAccess[ExecutionContext] {(m: Machine, k: ExecutionContext => MachineOp) =>
    k(m.cxt) // FIXME: Handle missing object.
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
    case _: VBool => TyBool
    case _: VStr => TyStr
    case _: VNum => TyNum
    case _: VObj => TyObj
    case _: VRef => TyRef
    case _: EnvironmentRecord => TyEnvRec
  }

  // GetBase(V) in spec
  def getBase(v: VRef): Val = v.base

  // IsUnresolvableReference(V) in spec
  def isUnresolvableReference(v: VRef): Boolean = (v.base == VUndef)

  // GetReferencedName(V) in spec
  def getReferencedName(v: VRef): String = v.refName

  // IsStrictReference(V) in spec
  def isStrictReference(v: VRef): Boolean = v.strictRef

  // IsPropertyReference(V) in spec
  def isPropertyReference(v: VRef): Boolean = v.base match {
    case _: VObj => true
    case _ => hasPrimitiveBase(v)
  }

  // HasPrimitiveBase(V) in spec
  def hasPrimitiveBase(v: VRef): Boolean = v.base match {
    case _: VBool => true
    case _: VStr => true
    case _: VNum => true
    case _ => false
  }

  // GetValue(V) in spec
  def getValue(v: Val): Val @cps[MachineOp] = v match {
    case v: VRef => {
      val base = getBase(v)
      if (isUnresolvableReference(v)) moThrow("ReferenceError") else moNop
      if (isPropertyReference(v)) {
        if (!hasPrimitiveBase(v)) {
          val baseData = load(base.asInstanceOf[VObj])
          baseData.get(getReferencedName(v))
        } else {
          val o = toObject(base)
          val odata = load(o)
          val descOption = odata.getProperty(getReferencedName(v))
          descOption match {
            case None => VUndef
            case Some(desc: DataProp) => desc.value
            case Some(desc: AccessorProp) => {
              val getter = desc.get
              if (getter == VUndef) {
                moVal(VUndef)
              } else {
                val getterData = load(base.asInstanceOf[VObj])
                getterData.asInstanceOf[CallableObj].call(base.asInstanceOf[VObj], Nil)
              }
            }
          }
        }
      } else {
        // base must be an environment record
        val baseEnvRec = base.asInstanceOf[EnvironmentRecord]
        getBindingValue(baseEnvRec, getReferencedName(v), isStrictReference(v))
      }
    }
    case _ => v
  }

  // envRec.GetBindingValue(N, S) in spec
  def getBindingValue(envRec: EnvironmentRecord, name: String, strict: Boolean): Val @cps[MachineOp] = envRec match {
    case envRec: DeclarativeEnvironmentRecord => {
      assert(envRec.bindings.contains(name))
      envRec.bindings(name) match {
        case MutableBinding(None, _) => VUndef // FIXME: Spec doesn't cover this situation. Spec error?
        case MutableBinding(Some(v), _) => v
        case ImmutableBinding(None) => if (strict) moThrow("ReferenceError") else VUndef
        case ImmutableBinding(Some(v)) => v
      }
    }
    case envRec: ObjectEnvironmentRecord => {
      val bindings = envRec.bindingObj
      val bindingsData = load(bindings)
      val value = bindingsData.hasProperty(name)
      if (!value) {
        if (!strict) VUndef
        moThrow("ReferenceError")
      } else moNop
      bindingsData.get(name)
    }
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

  def toObject(v: Val): VObj @cps[MachineOp] = v match {
    case VUndef => moThrow("TypeError")
    case VNull => moThrow("TypeError")
    case b: VBool => error("FIXME: Implement boolean boxing")
    case n: VNum => error("FIXME: Implement number boxing")
    case s: VStr => error("FIXME: Implement string boxing")
    case o: VObj => o
    case _ => error("Unsupported object conversion: " + v)
  }

  def isCallable(v: Val): Boolean @cps[MachineOp] = v match {
    case o: VObj => {
      val objData = load(o)
      objData.isInstanceOf[CallableObj]
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
      if (typ(func) != TyObj) moThrow("TypeError") else moNop
//    If IsCallable(func) is false, throw a TypeError exception.
      if (!isCallable(func)) moThrow("TypeError") else moNop
      val thisValue = if (typ(ref) == TyRef) {
//    If Type(ref) is Reference, then
        if (isPropertyReference(ref.asInstanceOf[VRef])) {
//        If IsPropertyReference(ref) is true, then
          getBase(ref.asInstanceOf[VRef])
//            Let thisValue be GetBase(ref).
        } else {
          val envRec = getBase(ref.asInstanceOf[VRef]).asInstanceOf[EnvironmentRecord]
//        Else, the base of ref is an Environment Record
          envRec.implicitThisValue
//            Let thisValue be the result of calling the ImplicitThisValue concrete method of GetBase(ref).
        }
      } else {
//    Else, Type(ref) is not Reference.
        VUndef
//        Let thisValue be undefined.
      }
      val funcData = load(func.asInstanceOf[VObj])
      funcData.asInstanceOf[CallableObj].call(thisValue, argList)
//    Return the result of calling the [[Call]] internal method on func, providing thisValue as the this value and providing the list argList as the argument values.
//The production CallExpression : CallExpression Arguments is evaluated in exactly the same manner, except that the contained CallExpression is evaluated in step 1.
    }
    case Identifier(name) => {
      val cxt = currentCxt
      val env = currentCxt.lexEnv
      val strict = isStrictModeCode(cxt.code)
      getIdentifierReference(Some(env), name, strict)
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
  
  sealed trait Code {
    def ses: List[SourceElement]
  }
  case class GlobalCode(ses: List[SourceElement]) extends Code
  case class EvalCode(ses: List[SourceElement], directStrictCall: Boolean) extends Code
  case class FunctionCode(func: ObjPtr, ses: List[SourceElement], declaredInStrict: Boolean) extends Code
  
  def isStrictModeCode(code: Code): Boolean = code match {
    case GlobalCode(ses) => hasUseStrictDirective(ses)
    case EvalCode(ses, directStrictCall) => hasUseStrictDirective(ses) || directStrictCall
    case FunctionCode(_, ses, declaredInStrict) => declaredInStrict || hasUseStrictDirective(ses)
  }

  // GetIdentifierReference(lex, name, strict) in spec
  def getIdentifierReference(lex: Option[LexicalEnvironment], name: String, strict: Boolean): VRef @cps[MachineOp] = {
    lex match {
      case None => VRef(VUndef, name, strict)
      case Some(lex) => {
        val envRec = lex.er
        val exists = envRec.hasBinding(name)
        if (exists) {
          moVal(VRef(envRec, name, strict))
        } else {
          val outer = lex.outer
          getIdentifierReference(outer, name, strict)
        }
      }
    }
  }

  def instantiateDeclarationBindings(args: List[Val]): Unit @cps[MachineOp] = {
    var cxt = currentCxt
    val env = cxt.varEnv.er
    val configurableBindings = cxt.code match {
      case _: EvalCode => true
      case _ => false
    }
    val strict = isStrictModeCode(cxt.code)
    cxt.code match {
      case FunctionCode(func, _, _) => {
        error("Not implemented")
//        val names = Nil.asInstanceOf[List[String]] //getInternalProperty(func, "[[FormalParameters]]")
//        val argCount = args.length
//        for ((argName, n) <- names.zipWithIndex) {
//          val v = if (n > argCount) VUndef else args(n)
//          val argAlreadyDeclared = hasBinding(env, argName)
//          if (!argAlreadyDeclared) createMutableBinding(env, argName)
//          setMutableBinding(env, v, strict)
//        }
      }
      case _ => ()
    }
//    cpsIterable(cxt.code.ses).cps.foldLeft(env) {
//      case (env0, FunctionDeclarationSourceElement(fd@FunctionDeclaration(fn, _, _))) => {
//        val fo = null // FIXME: Instantiate function
//        val funcAlreadyDeclared = env0.hasBinding(fn)
//        if (!funcAlreadyDeclared) { env0. }
//      }
//    }
    ()
  }

  def interpret(programSource: String): Completion = {
    val p = Parser.parse(programSource)

    val strictMode = hasUseStrictDirective(p.ses)
    if (p.ses.isEmpty) return Completion(CNormal, None, None) // Optimization from spec

    val globalCode = GlobalCode(p.ses)
    val globalObj = VObj(1)
    val heap = Heap(Map.empty.updated(1, NativeObj()), 2) // FIXME: Store global object data
    val globalEnv = LexicalEnvironment(ObjectEnvironmentRecord(globalObj), None)
    val progCxt = ExecutionContext(globalEnv, globalEnv, globalObj, globalCode)
    val m = Machine(progCxt, heap, globalObj, globalEnv)

    val (m1, c) = runMachineOp(m, reset {
      instantiateDeclarationBindings(Nil)
      val evalCompletion = evaluateSourceElements(p.ses)
      MOComp((_: Machine) => evalCompletion)
    })
    c
  }

}