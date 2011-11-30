package com.richdougherty.jsai

import scala.annotation.tailrec
import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.GenTraversableOnce
import scala.util.continuations._
import Parser._

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

  def assertionError(msg: String): Nothing =
    throw new java.lang.AssertionError(msg)

  final case class Cell[A](id: Int)
  final case class Heap(cells: Map[Int,Any], nextId: Int) {
    def this() = this(Map.empty, 1)
    def alloc[A]: (Heap, Cell[A]) =
      (new Heap(cells, nextId + 1), Cell[A](nextId))
    def free[A](cell: Cell[A]): Heap = {
      if (!cells.contains(cell.id))
        throw new NoSuchElementException("Cannot free non-existent cell: " + cell)
      new Heap(cells - cell.id, nextId)
    }
    def load[A](cell: Cell[A]): A =
      loadOption[A](cell) match {
        case None =>
          throw new NoSuchElementException("Cannot load non-existent cell: " + cell)
        case Some(a) => a
      }
    def loadOption[A](cell: Cell[A]): Option[A] =
      cells.get(cell.id).asInstanceOf[Option[A]]
    def store[A](cell: Cell[A], a: A): Heap =
      new Heap(cells.updated(cell.id, a), nextId)
  }

  sealed trait ValOrRef
  case class Ref(base: ValOrEnvRec, refName: String, strictRef: Boolean) extends ValOrRef

  sealed trait Typ
  case object TyUndef extends Typ
  case object TyNull extends Typ
  case object TyBool extends Typ
  case object TyStr extends Typ
  case object TyNum extends Typ
  case object TyObj extends Typ

  sealed trait Val extends ValOrRef with ValOrEnvRec
  case object VUndef extends Val
  case object VNull extends Val
  case class VBool(d: Boolean) extends Val
  case class VStr(d: String) extends Val
  case class VNum(d: Double) extends Val
  trait VObj extends Val {
    // Required methods
    def prototype: Option[VObj]
    def clazz: String
    def extensible: Boolean
    def get(propertyName: String): Val @cps[MachineOp]
    def getOwnProperty(propertyName: String): Option[Prop] @cps[MachineOp]
    def getProperty(propertyName: String): Option[Prop] @cps[MachineOp]
    def put(propertyName: String, v: Val, throwError: Boolean): Unit @cps[MachineOp]
    def canPut(propertyName: String): Boolean @cps[MachineOp]
    def hasProperty(propertyName: String): Boolean @cps[MachineOp]
    def delete(propertyName: String, throwError: Boolean): Boolean
    def defaultValue(hint: String): Val // returns primitive vals only
    def defineOwnProperty(propertyName: String, propDesc: PropDesc, throwError: Boolean): Boolean @cps[MachineOp]
    // Optional methods
    protected def ??? = error("Not implemented")
    def primitiveVal: Val = ???
    def `match`(s: String, index: Int): VObj = ???
    def parameterMap: VObj = ???
  }

  trait CallableObj {
    def call(thisObj: Val, args: List[Val]): ValOrRef @cps[MachineOp]
  }
  trait ConstructingObj {
    def construct(args: List[Val]): VObj @cps[MachineOp]
    def hasInstance(obj: Val): Boolean @cps[MachineOp]
  }
  trait BaseObj extends VObj {
    def props: Map[PropName, Prop] @cps[MachineOp]
    protected def setProps(newProps: Map[PropName, Prop]): Unit @cps[MachineOp]
    def extensible: Boolean = true
    def get(propertyName: String): Val @cps[MachineOp] = {
      val desc = getProperty(propertyName)
      desc match {
        case None => $(VUndef)
        case Some(dp: DataProp) => $(dp.value)
        case Some(ap: AccessorProp) => ap.get match {
          case VUndef => $(VUndef)
          case obj: VObj => {
            val callable = obj.asInstanceOf[CallableObj]
            callable.call(this, Nil).asInstanceOf[Val]
            // FIXME: Should spec throw a TypeError if getter returns a reference - or call getValue()?
          }
        }
      }
    }
    def getOwnProperty(propertyName: String): Option[Prop] @cps[MachineOp] = {
      props.get(propertyName) // Spec returns a copy of the (mutable) property descriptor
    }
    def getProperty(propertyName: String): Option[Prop] @cps[MachineOp] = {
      val propOption = getOwnProperty(propertyName)
      propOption match {
        case Some(_) => $(propOption)
        case None => {
          val protoOption = prototype
          protoOption match {
            case None => $(None)
            case Some(proto) => proto.getProperty(propertyName)
          }
        }
      }
    }
    def put(propertyName: String, v: Val, throwError: Boolean): Unit @cps[MachineOp] = {
      if (!canPut(propertyName)) {
        if (throwError) moThrow("TypeError") else $$
      } else {
        val ownDesc = getOwnProperty(propertyName)
        ownDesc match {
          case Some(dp: DataProp) => {
            val valueDesc = PropDesc(value = Some(v))
            defineOwnProperty(propertyName, valueDesc, throwError)
          }
          case _ => {
            val desc = getProperty(propertyName)
            desc match {
              case Some(ap: AccessorProp) => {
                val setter = ap.set // Spec says cannot be undefined
                setter.asInstanceOf[CallableObj].call(this, v::Nil)
              }
              case _ => {
                val newDesc = PropDesc(value = Some(v), writable = Some(true), enumerable = Some(true), configurable = Some(true))
                defineOwnProperty(propertyName, newDesc, throwError)
              }
            }
          }
        }
      }
      ()
    }
    def canPut(propertyName: String): Boolean @cps[MachineOp] = {
      val desc = getOwnProperty(propertyName)
      desc match {
        case Some(ap: AccessorProp) => $(ap.set != VUndef)
        case Some(dp: DataProp) => $(dp.writable)
        case _ => {
          val proto = prototype
          proto match {
            case None => $(extensible)
            case _ => {
              val inherited = getProperty(propertyName)
              inherited match {
                case None => extensible
                case Some(ap: AccessorProp) => (ap.set != VUndef)
                case Some(dp: DataProp) => dp.writable
              }
            }
          }
        }
      }
    }
    def hasProperty(propertyName: String): Boolean @cps[MachineOp] = {
      val desc = getProperty(propertyName)
      desc.isDefined
    }
    def delete(propertyName: String, throwError: Boolean): Boolean = ???
    def defaultValue(hint: String): Val = ???
    def defineOwnProperty(propertyName: String, desc: PropDesc, throwError: Boolean): Boolean @cps[MachineOp] = {
      val currentOpt = getOwnProperty(propertyName)

      def defaultDataProp = DataProp(
        DefaultPropValue.value,
        DefaultPropValue.writable,
        DefaultPropValue.enumerable,
        DefaultPropValue.configurable
      )
      def defaultAccessorProp = AccessorProp(
        DefaultPropValue.get,
        DefaultPropValue.set,
        DefaultPropValue.enumerable,
        DefaultPropValue.configurable
      )
      def updateDataProp(dp: DataProp, desc: PropDesc): DataProp = {
        DataProp(
          desc.value.getOrElse(dp.value),
          desc.writable.getOrElse(dp.writable),
          desc.enumerable.getOrElse(dp.enumerable),
          desc.configurable.getOrElse(dp.configurable)
        )
      }
      def updateAccessorProp(ap: AccessorProp, desc: PropDesc): AccessorProp = {
        AccessorProp(
          desc.get.getOrElse(ap.get),
          desc.get.getOrElse(ap.set),
          desc.enumerable.getOrElse(ap.enumerable),
          desc.configurable.getOrElse(ap.configurable)
        )
      }

      val proposed = if (isDataDescriptor(desc)) {
        val toUpdate = currentOpt match {
          case None => defaultDataProp
          case Some(dp: DataProp) => dp
          case Some(ap: AccessorProp) => DataProp(
            DefaultPropValue.value,
            DefaultPropValue.writable,
            ap.enumerable,
            ap.configurable
          )
        }
        updateDataProp(toUpdate, desc)
      } else if (isAccessorDescriptor(desc)) {
        val toUpdate = currentOpt match {
          case None => defaultAccessorProp
          case Some(dp: DataProp) => AccessorProp(
            DefaultPropValue.get,
            DefaultPropValue.set,
            dp.enumerable,
            dp.configurable
          )
          case Some(ap: AccessorProp) => ap
        }
        updateAccessorProp(toUpdate, desc)
      } else if (isGenericDescriptor(desc)) {
        val toUpdate = currentOpt.getOrElse(defaultDataProp)
        toUpdate match {
          case dp: DataProp => updateDataProp(dp, desc)
          case ap: AccessorProp => updateAccessorProp(ap, desc)
        }
      } else {
        error("Property descriptor must be either data, accessor or generic: " + desc)
      }

      // FIXME: Spec should reject when both are dataprop, configurable is false and writable is changed from true to false - but it doesn't!
      // We will reject, but need to check.
      sealed trait Action
      case object Update extends Action
      case object Reject extends Action
      case object Leave extends Action 

      def sameProp(p1: Prop, p2: Prop) = (p1, p2) match {
        case (dp1: DataProp, dp2: DataProp) => (
          sameValue(dp1.value, dp2.value) &&
          dp1.writable == dp2.writable &&
          dp1.enumerable == dp2.enumerable &&
          dp1.configurable == dp2.configurable
        )
        case (ap1: AccessorProp, ap2: AccessorProp) => (
          sameValue(ap1.get, ap2.get) &&
          sameValue(ap1.set, ap2.set) &&
          ap1.enumerable == ap2.enumerable &&
          ap1.configurable == ap2.configurable
        )
        case _ => false
      }
      def sameDataPropExceptValue(dp1: DataProp, dp2: DataProp) = (
        dp1.writable == dp2.writable &&
        dp1.enumerable == dp2.enumerable &&
        dp1.configurable == dp2.configurable
      )

      val action = (currentOpt, proposed) match {
        case (None, _) if extensible => Update
        case (None, _) => Reject
        case (Some(current), proposed) if (sameProp(current, proposed)) => Leave
        case (Some(current), _) if (current.configurable) => Update
        case (Some(current: DataProp), proposed: DataProp) if (current.writable && sameDataPropExceptValue(current, proposed)) => Update
        case _ => Reject
      }

      action match {
        case Update => {
          setProps(props.updated(propertyName, proposed))
          true
        }
        case Reject => {
          moThrow("TypeError")
          false
        }
        case Leave => {
          $(true)
        }
      }
    }
  }

  abstract class BasePropsObj(propsCell: Cell[Map[PropName, Prop]], val prototype: Option[VObj]) extends BaseObj {
      def props = @*(propsCell)
      def setProps(newProps: Map[PropName, Prop]) = propsCell @= newProps
  }

  case class NativeObj(propsCell: Cell[Map[PropName, Prop]], proto: Option[VObj]) extends BasePropsObj(propsCell, proto) {
    def clazz = "Object"
  }

  trait FunctionObj extends BaseObj with CallableObj with ConstructingObj {
    def clazz = "Function"
    override def get(propertyName: String): Val @cps[MachineOp] = {
      val v = super.get(propertyName)
      if (propertyName == "caller") {
        v match {
          case v: FunctionCodeObj if isStrictModeCode(v.code) => moThrow("TypeError")
          case _ => $$
        }
      } else $$
      v
    }
    def hasInstance(v: Val): Boolean @cps[MachineOp] = {
      // FIXME: This is pretty ugly.
      v match {
        case v: VObj => {
          val o = get("prototype")
          o match {
            case o: VObj => {
              def loop(v: VObj): Boolean = {
                val v2 = v.prototype
                v2 match {
                  case None => false
                  case Some(v3) => if (sameValue(o, v3)) {
                    true
                  } else {
                    loop(v3)
                  }
                }
              }
              loop(v)
            }
            case _ => moThrow("TypeError")
          }
        }
        case _ => $(false)
      }
    }
  }
  trait FunctionCodeObj extends FunctionObj {
    def code: FunctionCode
  }

  type PropName = String
  trait Prop {
    def enumerable: Boolean
    def configurable: Boolean
  }
  case class DataProp(
    value: Val,
    writable: Boolean,
    enumerable: Boolean,
    configurable: Boolean) extends Prop
  case class AccessorProp(
    get: Val,
    set: Val,
    enumerable: Boolean,
    configurable: Boolean) extends Prop

  object DefaultPropValue {
    def value = VUndef
    def get = VUndef
    def set = VUndef
    def writable = false
    def enumerable = false
    def configurable = false
  }

  final case class PropDesc(
    value: Option[Val] = None,
    writable: Option[Boolean] = None,
    enumerable: Option[Boolean] = None,
    configurable: Option[Boolean] = None,
    get: Option[Val] = None,
    set: Option[Val] = None
  ) {
    // Rule from spec
    assert(!(isDataDescriptor(this) && isAccessorDescriptor(this)))
  }

  final case class PropIdent(name: String, desc: PropDesc)

  case class Machine(cxt: ExecContext, ch: Option[CompletionHandler], heap: Heap, objs: MachineObjects, globalEnv: LexEnv)
  case class ExecContext(varEnv: LexEnv, lexEnv: LexEnv, thisBinding: VObj, code: Code)
  type CompletionHandler = (Machine, Completion) => (Machine, MachineOp)
  trait MachineObjects {
    def global: VObj
    def obj: VObj
    def func: VObj
  }

  sealed trait Code {
    def ses: List[SourceElement]
  }
  case class GlobalCode(ses: List[SourceElement]) extends Code
  case class EvalCode(ses: List[SourceElement], directStrictCall: Boolean) extends Code
  case class FunctionCode(params: List[String], ses: List[SourceElement], strict: Boolean) extends Code

  final case class AnnotatedStatement(stmt: Statement, labelSet: Set[Option[String]])

  case class LexEnv(er: EnvRec, outer: Option[LexEnv])

  sealed trait ValOrEnvRec
  sealed trait EnvRec extends ValOrEnvRec {
    def hasBinding(name: String): Boolean @cps[MachineOp]
    def createMutableBinding(name: String, canDelete: Boolean): Unit @cps[MachineOp]
    def setMutableBinding(name: String, v: Val, strict: Boolean): Unit @cps[MachineOp]
    def getBindingValue(name: String, strict: Boolean): Val @cps[MachineOp]
    def implicitThisValue: Val
  }
  case class DeclarativeEnvRec(bindings: Cell[Map[String, Binding]]) extends EnvRec {
    def hasBinding(name: String) = @*(bindings).contains(name)
    def createMutableBinding(name: String, canDelete: Boolean) = {
      assert(!hasBinding(name))
      bindings @= @*(bindings).updated(name, MutableBinding(@<(VUndef), canDelete))
    }
    def setMutableBinding(name: String, v: Val, strict: Boolean) = {
      val binding = @*(bindings).get(name)
      binding match {
        case None => assertionError("Binding missing: " + name)
        case Some(mb: MutableBinding) => { mb.v @= v }
        case Some(_: ImmutableBinding) => moThrow("ReferenceError")
      }
    }
    def getBindingValue(name: String, strict: Boolean): Val @cps[MachineOp] = {
      val binding = @*(bindings).get(name)
      binding match {
        case None => assertionError("Binding missing: " + name)
        case Some(mb: MutableBinding) => {
          @*(mb.v)
        }
        case Some(ib: ImmutableBinding) => {
          val v = @*(ib.v)
          v match {
            case None => if (strict) moThrow("ReferenceError") else $(VUndef)
            case Some(v) => $(v)
          }
        }
      }
    }
    def implicitThisValue: Val = VUndef
    def createImmutableBinding(name: String): Unit @cps[MachineOp] = {
      assert(!hasBinding(name))
      bindings @= @*(bindings).updated(name, ImmutableBinding(@<(None)))
    }
    def initializeImmutableBinding(name: String, v: Val): Unit @cps[MachineOp] = {
      val binding = @*(bindings).get(name)
      binding match {
        case None => $(assert(false))
        case Some(ib: ImmutableBinding) => { ib.v @= Some(v) }
        case Some(_: MutableBinding) => $(assert(false))
      }
    }
  }
  case class ObjectEnvRec(bindingObj: VObj, provideThis: Boolean = false) extends EnvRec {
    def hasBinding(name: String): Boolean @cps[MachineOp] = {
      bindingObj.hasProperty(name)
    }
    def createMutableBinding(name: String, canDelete: Boolean) = {
      val bindings = bindingObj
      assert(!bindings.hasProperty(name))
      val configValue = canDelete
      val propDesc = PropDesc(value = Some(VUndef), writable = Some(true), enumerable = Some(true), configurable = Some(configValue))
      bindings.defineOwnProperty(name, propDesc, true)
      ()
    }
    def setMutableBinding(name: String, v: Val, strict: Boolean) = {
      val bindings = bindingObj
      bindings.put(name, v, strict)
      ()
    }
    def getBindingValue(name: String, strict: Boolean): Val @cps[MachineOp] = {
      val bindings = bindingObj
      val value = bindings.hasProperty(name)
      if (!value) {
        if (!strict) VUndef
        moThrow("ReferenceError")
      } else $$
      bindings.get(name)
    }
    def implicitThisValue: Val = {
      if (provideThis) bindingObj else VUndef
    }
  }

  // FIXME: Make binding objects immutable, like properties
  sealed trait Binding
  case class MutableBinding(v: Cell[Val], canDelete: Boolean) extends Binding
  case class ImmutableBinding(v: Cell[Option[Val]]) extends Binding

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

  @tailrec
  final def runMachineOp(m: Machine, mo: MachineOp): (Machine, Completion) = mo match {
    case MOAccess(thunk) => {
      val mo1 = thunk(m)
      runMachineOp(m, mo1)
    }
    case MOUpdate(thunk) => {
      val (m1, mo1) = thunk(m)
      runMachineOp(m1, mo1)
    }
    case MOComp(thunk) => {
      m.ch match {
        case None => (m, thunk(m))
        case Some(handler) => {
          val c = thunk(m)
          val (m1, mo) = handler(m, c)
          runMachineOp(m1, mo)
        }
      }
    }
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

  def completionOf(thunk: => Nothing @cps[MachineOp]): Completion @cps[MachineOp] = {
    shift((k: Completion => MachineOp) => MOUpdate((m: Machine) => {
      val parentCompletionHandler = m.ch
      val completionHandler: CompletionHandler = (m: Machine, c: Completion) => {
        val m1 = m.copy(ch = parentCompletionHandler)
        (m1, k(c))
      }
      val m1 = m.copy(ch = Some(completionHandler))
      val mo = reset[Nothing,MachineOp](thunk)
      (m1, mo)
    }))
  }

  def moThrow(errorType: String): Nothing @cps[MachineOp] = {
    // FIXME: Create a proper error object
    val error = VStr(errorType + ": " + (new RuntimeException(errorType)).getStackTraceString)
    moComplete(Completion(CThrow, Some(error), None))
  }

  // Pass a value; used to work around continuations plugin bugs.
  def $[A](a: A) = moAccess[A] { (m: Machine, k: A => MachineOp) =>
    k(a) // FIXME: Handle missing object.
  }

  // Does nothing; used to work around continuations plugin bugs.
  def $$ = $(())

  def alloc[A] = moUpdate[Cell[A]] {(m: Machine, k: Cell[A] => MachineOp) =>
    val (h, cell) = m.heap.alloc[A]
    (m.copy(heap = h), k(cell))
  }

  def @<[A](a: A) = {
    // FIXME: Maybe interact with heap directly for speed?
    val cell = alloc[A]
    cell @= a
    cell
  }

  def @*[A](cell: Cell[A]) = moAccess[A] {(m: Machine, k: A => MachineOp) =>
    k(m.heap.load(cell)) // FIXME: Handle missing object.
  }

  def store[A](cell: Cell[A], a: A) = moUpdate[Unit] {(m: Machine, k: Unit => MachineOp) =>
    (m.copy(heap = m.heap.store(cell, a)), k(()))
  }

  trait CellSyntax[A] {
    def @=(a: A): Unit @cps[MachineOp]
  }

  implicit def cellSyntax[A](cell: Cell[A]): CellSyntax[A] = new CellSyntax[A] {
    def @=(a: A) = store[A](cell, a)
  }

  def currentCxt = moAccess[ExecContext] {(m: Machine, k: ExecContext => MachineOp) =>
    k(m.cxt)
  }

  def setCurrentCxt(cxt: ExecContext) = moUpdate[Unit] {(m: Machine, k: Unit => MachineOp) =>
    (m.copy(cxt = cxt), k(()))
  }

  def currentCompletionHandler = moAccess[Option[CompletionHandler]] {(m: Machine, k: Option[CompletionHandler] => MachineOp) =>
    k(m.ch)
  }

  def setCurrentCompletionHandler(ch: Option[CompletionHandler]) = moUpdate[Unit] {(m: Machine, k: Unit => MachineOp) =>
    (m.copy(ch = ch), k(()))
  }

  def getMachineObjs = moAccess[MachineObjects] {(m: Machine, k: MachineObjects => MachineOp) =>
    k(m.objs)
  }

  // FIXME: Remove this method?
  def getGlobalObj = getMachineObjs.global

  def getGlobalEnv = moAccess[LexEnv] {(m: Machine, k: LexEnv => MachineOp) =>
    k(m.globalEnv)
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
  }

  // GetBase(V) in spec
  def getBase(v: Ref): ValOrEnvRec = v.base

  // IsUnresolvableReference(V) in spec
  def isUnresolvableReference(v: Ref): Boolean = (v.base == VUndef)

  // GetReferencedName(V) in spec
  def getReferencedName(v: Ref): String = v.refName

  // IsStrictReference(V) in spec
  def isStrictReference(v: Ref): Boolean = v.strictRef

  // IsPropertyReference(V) in spec
  def isPropertyReference(v: Ref): Boolean = v.base match {
    case _: VObj => true
    case _ => hasPrimitiveBase(v)
  }

  // HasPrimitiveBase(V) in spec
  def hasPrimitiveBase(v: Ref): Boolean = v.base match {
    case _: VBool => true
    case _: VStr => true
    case _: VNum => true
    case _ => false
  }

  // GetValue(V) in spec
  def getValue(v: ValOrRef): Val @cps[MachineOp] = v match {
    case v: Ref => {
      val base = getBase(v)
      if (isUnresolvableReference(v)) moThrow("ReferenceError") else $$
      if (isPropertyReference(v)) {
        if (!hasPrimitiveBase(v)) {
          base.asInstanceOf[VObj].get(getReferencedName(v))
        } else {
          val o = toObject(base.asInstanceOf[Val])
          val descOption = o.getProperty(getReferencedName(v))
          descOption match {
            case None => $(VUndef)
            case Some(desc: DataProp) => $(desc.value)
            case Some(desc: AccessorProp) => {
              val getter = desc.get
              if (getter == VUndef) {
                $(VUndef)
              } else {
                getter.asInstanceOf[CallableObj].call(base.asInstanceOf[VObj], Nil).asInstanceOf[Val]
                // FIXME: Should spec throw a TypeError if getter returns a reference - or call getValue() recursively?
              }
            }
          }
        }
      } else {
        // base must be an environment record
        val baseEnvRec = base.asInstanceOf[EnvRec]
        baseEnvRec.getBindingValue(getReferencedName(v), isStrictReference(v))
      }
    }
    case v: Val => v
  }

  // PutValue(V, W) in spec
  def putValue(v: ValOrRef, w: Val): Unit @cps[MachineOp] = v match {
    case v: Ref => {
      val base = getBase(v)
      if (isUnresolvableReference(v)) {
        if (isStrictReference(v)) moThrow("ReferenceError") else $$
        val go = getGlobalObj
        go.put(getReferencedName(v), w, false)
      } else if (isPropertyReference(v)) {
        if (!hasPrimitiveBase(v)) {
          base.asInstanceOf[VObj].put(getReferencedName(v), w, isStrictReference(v))
        } else {
          val p = getReferencedName(v)
          val throwError = isStrictReference(v)
          val o = toObject(base.asInstanceOf[Val])
          if (!o.canPut(p)) {
            if (throwError) moThrow("TypeError") else $$
          } else {
            val ownDescOption = o.getOwnProperty(p)
            ownDescOption match {
              case Some(desc: DataProp) => if (throwError) moThrow("TypeError") else $$
              case _ => {
                val descOption = o.getProperty(p)
                descOption match {
                  case Some(desc: AccessorProp) => {
                    val setter = desc.set
                    if (setter == VUndef) {
                      $(assertionError("Setter cannot be undefined"))
                    } else {
                      setter.asInstanceOf[CallableObj].call(base.asInstanceOf[VObj], w::Nil)
                      // FIXME: Should spec throw a TypeError if setter returns a reference - or call getValue()?
                      ()
                    }
                  }
                  case _ => if (throwError) moThrow("TypeError") else $$
                }
              }
            }
          }
        }
      } else {
        // base must be an environment record
        val baseEnvRec = base.asInstanceOf[EnvRec]
        baseEnvRec.setMutableBinding(getReferencedName(v), w, isStrictReference(v))
      }
    }
    case _ => moThrow("ReferenceError")
  }

  // IsAccessorDescriptor(Desc) in spec
  def isAccessorDescriptor(desc: PropDesc): Boolean = {
    desc.get.isDefined || desc.set.isDefined
  }

  // IsDataDescriptor(Desc) in spec
  def isDataDescriptor(desc: PropDesc): Boolean = {
    desc.value.isDefined || desc.writable.isDefined
  }

  // IsGenericDescriptor(Desc) in spec
  def isGenericDescriptor(desc: PropDesc): Boolean = {
    !(isAccessorDescriptor(desc) || isDataDescriptor(desc))
  }

  // ToPrimitive(V) in spec
  def toPrimitive(v: Val, preferredType: Option[Typ] = None): Val = v match {
    case _: VObj => error("toPrimitive() not yet implemented for objects")
    case _ => v
  }

  // ToBoolean(V) in spec
  def toBoolean(v: Val): VBool = v match {
    case VUndef => VBool(false)
    case VNull => VBool(false)
    case b: VBool => b
    case n: VNum => VBool(!(n.d == 0.0 || n.d.isNaN))
    case s: VStr => VBool(!s.d.isEmpty)
    case _: VObj => VBool(true)
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
    case o: VObj => o.isInstanceOf[CallableObj]
    case _ => false
  }

  def evaluateVariableDeclaration(decl: VariableDeclaration): String @cps[MachineOp] = decl match {
    case VariableDeclaration(ident, None) => ident
    case VariableDeclaration(ident, Some(initialiser)) => {
      val lhs = evaluateExpression(Identifier(ident))
      val rhs = evaluateExpression(initialiser)
      val value = getValue(rhs)
      putValue(lhs, value)
      ident
    }
  }

  def evaluateReusableOperation(lval: Val, op: ReusableOperator, rval: Val): Val @cps[MachineOp] = op match {
    case AdditionOperator => {
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

  def guardAssignment(lref: ValOrRef): Unit @cps[MachineOp] = {
    lref match {
      case Ref(_: EnvRec, "eval" | "arguments", true) => moThrow("SyntaxError")
      case _ => $$
    }
  }

  def abstractRelationalComparison(x: Val, y: Val, leftFirst: Boolean): Option[Boolean] @cps[MachineOp] = {
    val (px, py) = if (leftFirst) {
      (toPrimitive(x), toPrimitive(y))
    } else {
      (toPrimitive(y), toPrimitive(x))
    }
    (px, py) match {
      case (px: VStr, py: VStr) => {
        Some(px.d < py.d)
      }
      case _ => {
        val nx = toNumber(px)
        val ny = toNumber(py)
        // FIXME: Make sure that the JVM's semantics match ECMAScript's
        val lt = nx.d < ny.d
        val gt = nx.d > ny.d
        if (lt == gt) None else Some(lt)
      }
    }
  }

  def evaluateExpression(expr: Expression): ValOrRef @cps[MachineOp] = expr match {
    case PrefixExpression(op, rexpr) => op match {
      case NegativeOperator => {
        val expr = evaluateExpression(rexpr)
        val oldValue = toNumber(getValue(expr))
        VNum(-1 * oldValue.d)
      }
    }
    case InfixExpression(lexpr, op, rexpr) => op match {
      case op@AdditionOperator => {
        val lref = evaluateExpression(lexpr)
        val lval = getValue(lref)
        val rref = evaluateExpression(rexpr)
        val rval = getValue(rref)
        evaluateReusableOperation(lval, op, rval)
      }
      case op@LessThanOperator => {
        val lref = evaluateExpression(lexpr)
        val lval = getValue(lref)
        val rref = evaluateExpression(rexpr)
        val rval = getValue(rref)
        val r = abstractRelationalComparison(lval, rval, true)
        VBool(r.getOrElse(false))
      }
      case SimpleAssignmentOperator => {
        val lref = evaluateExpression(lexpr)
        val rref = evaluateExpression(rexpr)
        val rval = getValue(rref)
        guardAssignment(lref)
        putValue(lref, rval)
        rval
      }
      case CompoundAssignmentOperator(compoundOp) => {
        val lref = evaluateExpression(lexpr)
        val lval = getValue(lref)
        val rref = evaluateExpression(rexpr)
        val rval = getValue(rref)
        val r = evaluateReusableOperation(lval, compoundOp, rval)
        guardAssignment(lref)
        putValue(lref, r)
        r
      }
    }
    case PostfixExpression(l, op) => op match {
      case IncrementOperator => {
        val lhs = evaluateExpression(l)
        guardAssignment(lhs)
        val oldValue = toNumber(getValue(lhs))
        val newValue = evaluateReusableOperation(oldValue, AdditionOperator, VNum(1))
        putValue(lhs, newValue)
        oldValue
      }
    }
    case BooleanLiteral(b) => VBool(b)
    case NumericLiteral(d) => VNum(d)
    case StringLiteral(s) => VStr(s)
    case ObjectInitialiser(pas) => {
      def evaluatePropAssignName(pan: PropAssignName): String = pan match {
        case IdentifierPropAssignName(n) => n
      }
      def evaluatePropAssign(pa: PropAssign): PropIdent @cps[MachineOp] = pa match {
        case ValuePropAssign(pan, expr) => {
          val propName = evaluatePropAssignName(pan)
          val exprValue = evaluateExpression(expr)
          val propVal = getValue(exprValue)
          val desc = PropDesc(
              value = Some(propVal),
              writable = Some(true),
              enumerable = Some(true),
              configurable = Some(true))
          PropIdent(propName, desc)
        }
      }
      val obj = newEmptyObj
      for (pa <- pas.cps) {
        val propId = evaluatePropAssign(pa)
        obj.defineOwnProperty(propId.name, propId.desc, false)
      }
      obj
    }
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
      if (typ(func) != TyObj) moThrow("TypeError") else $$
//    If IsCallable(func) is false, throw a TypeError exception.
      if (!isCallable(func)) moThrow("TypeError") else $$
      val thisValue = ref match {
        case ref: Ref => {
//    If Type(ref) is Reference, then
          if (isPropertyReference(ref)) {
//        If IsPropertyReference(ref) is true, then
            getBase(ref).asInstanceOf[Val] // Either VObj, VNum, VStr, VBool
//            Let thisValue be GetBase(ref).
          } else {
            val envRec = getBase(ref.asInstanceOf[Ref]).asInstanceOf[EnvRec]
//        Else, the base of ref is an Environment Record
            envRec.implicitThisValue
//            Let thisValue be the result of calling the ImplicitThisValue concrete method of GetBase(ref).
          }
        }
        case _: Val => {
//    Else, Type(ref) is not Reference.
          VUndef
//        Let thisValue be undefined.
        }
      }
      func.asInstanceOf[CallableObj].call(thisValue, argList)
//    Return the result of calling the [[Call]] internal method on func, providing thisValue as the this value and providing the list argList as the argument values.
//The production CallExpression : CallExpression Arguments is evaluated in exactly the same manner, except that the contained CallExpression is evaluated in step 1.
    }
    case Identifier(name) => {
      val cxt = currentCxt
      val env = currentCxt.lexEnv
      val strict = isStrictModeCode(cxt.code)
      getIdentifierReference(Some(env), name, strict)
    }
    case ConditionalExpression(testExpr, trueExpr, falseExpr) => {
      val lref = evaluateExpression(testExpr)
      val lbool = toBoolean(getValue(lref))
      if (lbool.d) {
        val trueRef = evaluateExpression(trueExpr)
        getValue(trueRef)
      } else {
        val falseRef = evaluateExpression(falseExpr)
        getValue(falseRef)
      }
    }
  }

  def evaluateSourceElement(se: SourceElement): Nothing @cps[MachineOp] = {
    // FIXME: Throw error break and continue?
    se match {
      case StatementSourceElement(st) => evaluateStatement(AnnotatedStatement(st, Set.empty))
      case FunctionDeclarationSourceElement(_) =>
        moComplete(Completion(CNormal, None, None))
    }
  }

  def evaluateStatement(as: AnnotatedStatement): Nothing @cps[MachineOp] = {
    val c = as.stmt match {
      case BlockStatement(stmts) => {
        def loop(stmts: List[Statement], prevComp: Completion): Nothing @cps[MachineOp] = stmts match {
          case Nil => moComplete(prevComp)
          case headStmt::rest => {
            val s = completionOf(evaluateStatement(AnnotatedStatement(headStmt, Set.empty)))
            s match {
              case Completion(CThrow, v, _) => moComplete(Completion(CThrow, v, None))
              case Completion(CNormal, v, target) => {
                val c = Completion(CNormal, if (v.isDefined) v else prevComp.v, target)
                loop(rest, c)
              }
              case _ => moComplete(s)
            }
          }
        }
        loop(stmts, Completion(CNormal, None, None))
      }
      case VariableStatement(decls) => {
        evaluateVariableDeclarationList(decls)
        Completion(CNormal, None, None)
      }
      case ExpressionStatement(expr) => {
        val exprRef = evaluateExpression(expr)
        Completion(CNormal, Some(getValue(exprRef)), None)
      }
      case IfStatement(testExpr, trueStmt, falseStmt) => {
        val exprRef = evaluateExpression(testExpr)
        val exprBool = toBoolean(getValue(exprRef))
        if (exprBool.d) {
          evaluateStatement(AnnotatedStatement(trueStmt, Set.empty))
        } else {
          falseStmt match {
            case None => moComplete(Completion(CNormal, None, None))
            case Some(falseStmt) => evaluateStatement(AnnotatedStatement(falseStmt, Set.empty))
          }
        }
      }
      case ForStatement(init, testExpr, incrExpr, forStmt) => {
        init match {
          case Left(None) => $(None)
          case Left(Some(expr)) => {
            val exprRef = evaluateExpression(expr)
            getValue(exprRef)
          }
          case Right(decls) => {
            evaluateVariableDeclarationList(decls)
          }
        }
        def loop(v: Option[Val]): Nothing @cps[MachineOp] = {
          if (testExpr.isDefined) {
            val testExprRef = evaluateExpression(testExpr.get)
            val testExprBool = toBoolean(getValue(testExprRef))
            if (!(testExprBool.d)) moComplete(Completion(CNormal, v, None)) else $$
          } else $$
          val stmtComp = completionOf(evaluateStatement(AnnotatedStatement(forStmt, Set.empty)))
          val v2 = if (stmtComp.v.isDefined) stmtComp.v else v
          stmtComp match {
            case Completion(CBreak, _, target) if as.labelSet.contains(target) =>
              moComplete(Completion(CNormal, v2, None))
            case Completion(CBreak | CThrow | CReturn, _, _) =>
              moComplete(stmtComp)
            case Completion(CContinue, _, target) if !as.labelSet.contains(target) =>
              moComplete(stmtComp)
            case Completion(CNormal | CContinue, _, _) => {
              if (incrExpr.isDefined) {
                val incrRefExpr = evaluateExpression(incrExpr.get)
                getValue(incrRefExpr)
              } else $$
              loop(v2)
            }
          }
        }
        completionOf(loop(None))
      }
      case ReturnStatement(expr) => expr match {
        case None => Completion(CReturn, None, None)
        case Some(expr0) => {
          val exprRef = evaluateExpression(expr0)
          Completion(CReturn, Some(getValue(exprRef)), None)
        }
      }
      case LabelledStatement(label, childStmt) => {
        val initialLabelSet: Set[Option[String]] = childStmt match {
          case _: IterationStatement => Set.empty + None
          case _: SwitchStatement => Set.empty + None
          case _ => Set.empty
        }
        evaluateStatement(AnnotatedStatement(
          childStmt,
          initialLabelSet + Some(label) ++ as.labelSet
        ))
      }
    }
    moComplete(c)
  }

  def evaluateVariableDeclarationList(decls: List[VariableDeclaration]): Unit @cps[MachineOp] = {
    for (decl <- decls.cps) {
      evaluateVariableDeclaration(decl)
    }
  }


  def evaluateSourceElements(ses: List[SourceElement]): Nothing @cps[MachineOp] = {
    val c = ses.cps.foldLeft(Completion(CNormal, None, None)) {
      case (Completion(CNormal, prevVal, prevTarget), se) => {
        val currCompletion = completionOf(evaluateSourceElement(se))
        if (currCompletion.v.isDefined) currCompletion else currCompletion.copy(v = prevVal)
      }
      case (abrupt, _) => abrupt
    }
    moComplete(c)
  }

  // SameValue(X, Y) in spec
  def sameValue(x: Val, y: Val): Boolean = (x, y) match {
    case (_: VObj, _: VObj) => x eq y
    case _ => x == y
  }

  def isStrictModeCode(code: Code): Boolean = code match {
    case GlobalCode(ses) => hasUseStrictDirective(ses)
    case EvalCode(ses, directStrictCall) => hasUseStrictDirective(ses) || directStrictCall
    case FunctionCode(_, ses, declaredInStrict) => declaredInStrict || hasUseStrictDirective(ses)
  }

  // NewDeclarativeEnvironment(E) in spec
  def newDeclarativeEnvironment(outer: Option[LexEnv]): LexEnv @cps[MachineOp] = {
    val envRec = DeclarativeEnvRec(@<(Map.empty))
    LexEnv(envRec, outer)
  }

  // Steps 3-7 of "new Object([value])" in spec
  def newEmptyObj: VObj @cps[MachineOp] = {
    NativeObj(@<(Map.empty), Some(getMachineObjs.obj.getProperty("prototype")))
 }

  // Based on "Function Definition" section in spec
  def newFunctionObjFromDeclaration(fd: FunctionDeclaration, lexEnv: LexEnv, strict: Boolean): VObj @cps[MachineOp] = {
    val funcEnv = newDeclarativeEnvironment(Some(lexEnv))
    val envRec = funcEnv.er.asInstanceOf[DeclarativeEnvRec]
    envRec.createImmutableBinding(fd.ident)
    val closure = newFunctionObj(fd.params, fd.body, funcEnv, strict || hasUseStrictDirective(fd.body))
    envRec.initializeImmutableBinding(fd.ident, closure)
    closure
  }

  // Based on "Creating Function Objects" section in spec
  def newFunctionObj(params: List[String], body: List[SourceElement], scope: LexEnv, inStrict: Boolean): VObj @cps[MachineOp] = {
    val propsCell = @<(Map.empty[PropName, Prop])
    val funcProto = getMachineObjs.func.prototype
    val f = new BasePropsObj(propsCell, funcProto) with FunctionCodeObj {
//    Set all the internal methods, except for [[Get]], of F as described in 8.12.
//    Set the [[Class]] internal property of F to "Function".
//    Set the [[Prototype]] internal property of F to the standard built-in Function prototype object as specified in 15.3.3.1.
//    Set the [[Get]] internal property of F as described in 15.3.5.4.
//    Set the [[Call]] internal property of F as described in 13.2.1.
      def call(thisArg: Val, args: List[Val]): ValOrRef @cps[MachineOp] = {
        val strict = isStrictModeCode(code)
        val thisBinding = if (strict) {
          thisArg
        } else if (thisArg == VNull || thisArg == VUndef) {
          getGlobalObj
        } else if (typ(thisArg) != TyObj) {
          toObject(thisArg)
        } else {
          thisArg
        }
        val localEnv = newDeclarativeEnvironment(Some(scope))
        val cxt = ExecContext(localEnv, localEnv, thisBinding.asInstanceOf[VObj], code)
        val parentCxt = currentCxt
        setCurrentCxt(cxt)
        instantiateDeclarationBindings(args)
        
        val c = completionOf(evaluateSourceElements(code.ses))
        setCurrentCxt(parentCxt)
        c match {
          case Completion(CNormal, _, None) => $(VUndef)
          case Completion(CBreak | CContinue, _, _) =>
            assertionError("Illegal completion for function: " + c)
          case Completion(CReturn, None, None) => $(VUndef)
          case Completion(CReturn, Some(v), None) => $(v)
          case Completion(CThrow, _, _) => moComplete(c)
        }
      }
//    Set the [[Construct]] internal property of F as described in 13.2.2.
      def construct(args: List[Val]): VObj @cps[MachineOp] = {
        val proto = get("prototype")
        val fixedProto = proto match {
          case proto: VObj => $(proto)
          case _ => getMachineObjs.obj.prototype.get
        }
        val obj = NativeObj(@<(Map.empty[PropName, Prop]), Some(fixedProto))
        val result = call(obj, args)
        result match {
          case result: VObj => result
          case _ => obj
        }
      }
//    Set the [[HasInstance]] internal property of F as described in 15.3.5.3.
//    Set the [[Scope]] internal property of F to the value of Scope.
//    Let names be a List containing, in left to right textual order, the Strings corresponding to the identifiers of FormalParameterList.
//    Set the [[FormalParameters]] internal property of F to names.
//    Set the [[Code]] internal property of F to FunctionBody.
      val code = FunctionCode(params, body, inStrict)
//    Set the [[Extensible]] internal property of F to true.
    }
//    Create a new native ECMAScript object and let F be that object.

//    Let len be the number of formal parameters specified in FormalParameterList. If no parameters are specified, let len be 0.
//    Call the [[DefineOwnProperty]] internal method of F with arguments "length", Property Descriptor {[[Value]]: len, [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false}, and false.
    val len = VNum(params.length)
    f.defineOwnProperty("length", PropDesc(
        value = Some(len),
        writable = Some(false),
        enumerable = Some(false),
        configurable = Some(false)), false)

//    Let proto be the result of creating a new object as would be constructed by the expression new Object()where Object is the standard built-in constructor with that name.
//    Call the [[DefineOwnProperty]] internal method of proto with arguments "constructor", Property Descriptor {[[Value]]: F, { [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true}, and false.
//    Call the [[DefineOwnProperty]] internal method of F with arguments "prototype", Property Descriptor {[[Value]]: proto, { [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false}, and false.
    val proto = newEmptyObj
    proto.defineOwnProperty("constructor", PropDesc(
        value = Some(f),
        writable = Some(true),
        enumerable = Some(false),
        configurable = Some(true)), false)
    f.defineOwnProperty("prototype", PropDesc(
        value = Some(proto),
        writable = Some(true),
        enumerable = Some(false),
        configurable = Some(true)), false)

//    If Strict is true, then
//
//        Let thrower be the [[ThrowTypeError]] function Object (13.2.3).
//
//        Call the [[DefineOwnProperty]] internal method of F with arguments "caller", PropertyDescriptor {[[Get]]: thrower, [[Set]]: thrower, [[Enumerable]]: false, [[Configurable]]: false}, and false.
//
//        Call the [[DefineOwnProperty]] internal method of F with arguments "arguments", PropertyDescriptor {[[Get]]: thrower, [[Set]]: thrower, [[Enumerable]]: false, [[Configurable]]: false}, and false.
//
    f
  }

  // GetIdentifierReference(lex, name, strict) in spec
  def getIdentifierReference(lex: Option[LexEnv], name: String, strict: Boolean): Ref @cps[MachineOp] = {
    lex match {
      case None => Ref(VUndef, name, strict)
      case Some(lex) => {
        val envRec = lex.er
        val exists = envRec.hasBinding(name)
        if (exists) {
          $(Ref(envRec, name, strict))
        } else {
          val outer = lex.outer
          getIdentifierReference(outer, name, strict)
        }
      }
    }
  }

  // Based on "Declaration Binding Instantiation" section in spec
  def instantiateDeclarationBindings(args: List[Val]): Unit @cps[MachineOp] = {
    var cxt = currentCxt
    val env = cxt.varEnv.er
    val configurableBindings = cxt.code match {
      case _: EvalCode => true
      case _ => false
    }
    val strict = isStrictModeCode(cxt.code)
    cxt.code match {
      case FunctionCode(names, _, _) => {
        val argCount = args.length
        var n = 0 // Manually increment because zipWithIndex didn't play with cps
        for (argName <- names.cps) {
          val v = if (n > argCount) VUndef else args(n)
          val argAlreadyDeclared = env.hasBinding(argName)
          if (!argAlreadyDeclared) env.createMutableBinding(argName, false) else $$ // FIXME: Spec doesn't specify value of canDelete
          env.setMutableBinding(argName, v, strict)
          n += 1
        }
      }
      case _ => $(())
    }
    cpsIterable(cxt.code.ses).cps.foreach {
      case FunctionDeclarationSourceElement(fd@FunctionDeclaration(fn, _, _)) => {
        val fo = newFunctionObjFromDeclaration(fd, cxt.lexEnv, strict) // FIXME: Instantiate function
        val funcAlreadyDeclared = env.hasBinding(fn)
        if (!funcAlreadyDeclared) {
          env.createMutableBinding(fn, configurableBindings)
        } else if (!cxt.varEnv.outer.isDefined) {
          val go = getGlobalObj
          val existingProp = go.getProperty(fn).get
          if (existingProp.configurable) {
            val propDesc = PropDesc(value = Some(VUndef), writable = Some(true), enumerable = Some(true), configurable = Some(configurableBindings))
            go.defineOwnProperty(fn, propDesc, true)
          } else if (
            existingProp match {
              case _: AccessorProp => true
              case dp: DataProp if !(dp.writable && dp.enumerable) => true
              case _ => false
            }
          ) {
            moThrow("TypeError")
          } else $$
        } else $$
        env.setMutableBinding(fn, fo, strict)
      }
      case _ => ()
    }
    ()
  }

  def startingHeap: (Heap, MachineObjects) = {
    def newObj(h: Heap, constructor: Cell[Map[PropName,Prop]] => VObj): (Heap, VObj) = {
      val (h1, props) = h.alloc[Map[PropName,Prop]]
      val h2 = h1.store(props, Map.empty[PropName, Prop])
      val obj = constructor(props)
      (h2, obj)
    }
    val h1 = new Heap()
    // FIXME: Global obj's class and prototype is impl dependent - add config switches?
    val (h2, globalObj) = newObj(h1, NativeObj(_, None))
    val (h3, objProto) = newObj(h2, NativeObj(_, None))
    val (h4, objObj) = newObj(h3, NativeObj(_, Some(objProto)))
    val (h5, funcProto) = newObj(h4, (propsCell: Cell[Map[PropName,Prop]]) => new BasePropsObj(propsCell, Some(objProto)) with CallableObj {
      def clazz = "Function"
      def call(thisArg: Val, args: List[Val]): ValOrRef @cps[MachineOp] = VUndef
    })
    val (h6, funcObj) = newObj(h5, (propsCell: Cell[Map[PropName,Prop]]) => new BasePropsObj(propsCell, Some(objProto)) with CallableObj {
      def clazz = "Function"
      def call(thisArg: Val, args: List[Val]): ValOrRef @cps[MachineOp] = ???
    })
    val mos = new MachineObjects {
      val global = globalObj
      val obj = objObj
      val func = funcObj
    }
    (h4, mos)
  }

  def interpret(programSource: String): Completion = {
    val p = Parser.parse(programSource)

    val strictMode = hasUseStrictDirective(p.ses)
    if (p.ses.isEmpty) return Completion(CNormal, None, None) // Optimization from spec

    val globalCode = GlobalCode(p.ses)
    val (h, mos) = startingHeap
    val globalEnv = LexEnv(ObjectEnvRec(mos.global), None)
    val progCxt = ExecContext(globalEnv, globalEnv, mos.global, globalCode)
    val m = Machine(progCxt, None, h, mos, globalEnv)

    val (m1, c) = runMachineOp(m, reset {
      m.objs.global.defineOwnProperty("undefined", PropDesc(
          value = Some(VUndef),
          writable = Some(false),
          enumerable = Some(false),
          configurable = Some(false)
      ), true)
      instantiateDeclarationBindings(Nil)
      evaluateSourceElements(p.ses)
    })
    c
  }

}