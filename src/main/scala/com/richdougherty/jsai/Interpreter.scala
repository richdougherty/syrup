package com.richdougherty.jsai

import scala.annotation.tailrec
import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.GenTraversableOnce
import scala.util.continuations._
import Parser.AdditionOperator
import Parser.CallExpression
import Parser.Expression
import Parser.ExpressionStatement
import Parser.Identifier
import Parser.FunctionDeclaration
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

  final case class Cell[A](id: Int)
  final class Heap(cells: Map[Int,Any], nextId: Int) {
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

  sealed trait Val extends ValOrRef with ValOrEnvRec

  case object VUndef extends Val
  case object VNull extends Val
  case class VBool(d: Boolean) extends Val
  case class VStr(d: String) extends Val
  case class VNum(d: Double) extends Val
  case class VObj(cell: Cell[ObjData]) extends Val

  sealed trait Typ
  case object TyUndef extends Typ
  case object TyNull extends Typ
  case object TyBool extends Typ
  case object TyStr extends Typ
  case object TyNum extends Typ
  case object TyObj extends Typ

  type ObjPtr = Int
  trait ObjData {
    def thisVal: VObj
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
    def construct(args: List[Val]): VObj = ???
    def hasInstance(obj: Val): Boolean = ???
    def scope: LexEnv = ???
    def formalParameters: List[String] = ???
    def code: FunctionCode = ???
    def targetFunction: VObj = ???
    def boundThis: VObj = ???
    def boundArguments: List[Val] = ???
    def `match`(s: String, index: Int): VObj = ???
    def parameterMap: VObj = ???
  }
  trait CallableObj {
    def call(thisObj: Val, args: List[Val]): ValOrRef @cps[MachineOp]
  }
  trait BaseObj extends ObjData {
    def thisVal: VObj
    def props: Cell[Map[PropName, Prop]]
    def clazz: String = ???
    def extensible: Boolean = true
    def get(propertyName: String): Val @cps[MachineOp] = {
      val desc = getProperty(propertyName)
      desc match {
        case None => moVal(VUndef)
        case Some(dp: DataProp) => moVal(dp.value)
        case Some(ap: AccessorProp) => ap.get match {
          case VUndef => moVal(VUndef)
          case obj: VObj => {
            val callable = @*(obj.cell).asInstanceOf[CallableObj]
            callable.call(thisVal, Nil).asInstanceOf[Val]
            // FIXME: Should spec throw a TypeError if getter returns a reference - or call getValue()?
          }
        }
      }
    }
    def getOwnProperty(propertyName: String): Option[Prop] @cps[MachineOp] = {
      @*(props).get(propertyName) // Spec returns a copy of the (mutable) property descriptor
    }
    def getProperty(propertyName: String): Option[Prop] @cps[MachineOp] = {
      val propOption = getOwnProperty(propertyName)
      propOption match {
        case Some(_) => moVal(propOption)
        case None => {
          val protoOption = prototype
          protoOption match {
            case None => moVal(None)
            case Some(proto) => {
              val protoData = @*(proto.cell)
              protoData.getProperty(propertyName)
            }
          }
        }
      }
    }
    def put(propertyName: String, v: Val, throwError: Boolean): Unit @cps[MachineOp] = {
      if (!canPut(propertyName)) {
        if (throwError) moThrow("TypeError") else moNop
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
                val setter = @*(ap.set.asInstanceOf[VObj].cell) // Spec says cannot be undefined
                setter.asInstanceOf[CallableObj].call(thisVal, v::Nil)
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
        case Some(ap: AccessorProp) => moVal(ap.set != VUndef)
        case Some(dp: DataProp) => moVal(dp.writable)
        case _ => {
          val proto = prototype
          proto match {
            case None => moVal(extensible)
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
      
      val action = (currentOpt, proposed) match {
        case (None, _) if extensible => Update
        case (None, _) => Reject
        case (Some(current), proposed) if (current == proposed) => Leave
        case (Some(current), _) if (current.configurable) => Update
        case (Some(current: DataProp), proposed: DataProp) if (current.writable && current == proposed.copy(value = current.value)) => Update
        case _ => Reject
      }

      action match {
        case Update => {
          props @= @*(props).updated(propertyName, proposed)
          true
        }
        case Reject => {
          moThrow("TypeError")
          false
        }
        case Leave => {
          moVal(true)
        }
      }
    }
  }

  case class NativeObj(thisVal: VObj, props: Cell[Map[PropName, Prop]], prototype: Option[VObj]) extends BaseObj

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

  case class Machine(cxt: ExecContext, heap: Heap, globalObj: VObj, globalEnv: LexEnv)
  case class ExecContext(varEnv: LexEnv, lexEnv: LexEnv, thisBinding: VObj, code: Code)

  sealed trait Code {
    def ses: List[SourceElement]
  }
  case class GlobalCode(ses: List[SourceElement]) extends Code
  case class EvalCode(ses: List[SourceElement], directStrictCall: Boolean) extends Code
  case class FunctionCode(func: VObj, ses: List[SourceElement], strict: Boolean) extends Code

  case class LexEnv(er: EnvRec, outer: Option[LexEnv])

  sealed trait ValOrEnvRec
  // FIXME: Rename to EnvRec
  sealed trait EnvRec extends ValOrEnvRec {
    def hasBinding(name: String): Boolean @cps[MachineOp]
    def createMutableBinding(name: String, canDelete: Boolean): Unit @cps[MachineOp]
    def setMutableBinding(name: String, v: Val, strict: Boolean): Unit @cps[MachineOp]
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
        case None => assert(false)
        case Some(mb: MutableBinding) => { mb.v @= v }
        case Some(_: ImmutableBinding) => moThrow("ReferenceError")
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
        case None => moVal(assert(false))
        case Some(ib: ImmutableBinding) => { ib.v @= Some(v) }
        case Some(_: MutableBinding) => moVal(assert(false))
      }
    }
  }
  case class ObjectEnvRec(bindingObj: VObj, provideThis: Boolean = false) extends EnvRec {
    def hasBinding(name: String): Boolean @cps[MachineOp] = {
      val bindingObjData = @*(bindingObj.cell)
      bindingObjData.hasProperty(name)
    }
    def createMutableBinding(name: String, canDelete: Boolean) = {
      val bindings = @*(bindingObj.cell)
      assert(!bindings.hasProperty(name))
      val configValue = canDelete
      val propDesc = PropDesc(value = Some(VUndef), writable = Some(true), enumerable = Some(true), configurable = Some(configValue))
      bindings.defineOwnProperty(name, propDesc, true)
      ()
    }
    def setMutableBinding(name: String, v: Val, strict: Boolean) = {
      val bindings = @*(bindingObj.cell)
      bindings.put(name, v, strict)
      ()
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

  def alloc[A] = moUpdate[Cell[A]] {(m: Machine, k: Cell[A] => MachineOp) =>
    val (h, cell) = m.heap.alloc[A]
    (m.copy(heap = h), k(cell))
  }

  def newObj = {
    val cell = alloc[ObjData]
    VObj(cell)
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

  def getGlobalObj = moAccess[VObj] {(m: Machine, k: VObj => MachineOp) =>
    k(m.globalObj)
  }

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
      if (isUnresolvableReference(v)) moThrow("ReferenceError") else moNop
      if (isPropertyReference(v)) {
        if (!hasPrimitiveBase(v)) {
          val baseData = @*(base.asInstanceOf[VObj].cell)
          baseData.get(getReferencedName(v))
        } else {
          val o = toObject(base.asInstanceOf[Val])
          val odata = @*(o.cell)
          val descOption = odata.getProperty(getReferencedName(v))
          descOption match {
            case None => VUndef
            case Some(desc: DataProp) => desc.value
            case Some(desc: AccessorProp) => {
              val getter = desc.get
              if (getter == VUndef) {
                moVal(VUndef)
              } else {
                val getterData = @*(base.asInstanceOf[VObj].cell)
                getterData.asInstanceOf[CallableObj].call(base.asInstanceOf[VObj], Nil).asInstanceOf[Val]
                // FIXME: Should spec throw a TypeError if getter returns a reference - or call recursively?
              }
            }
          }
        }
      } else {
        // base must be an environment record
        val baseEnvRec = base.asInstanceOf[EnvRec]
        getBindingValue(baseEnvRec, getReferencedName(v), isStrictReference(v))
      }
    }
    case v: Val => v
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

  // FIXME: Make a normal method of EnvRec
  // envRec.GetBindingValue(N, S) in spec
  def getBindingValue(envRec: EnvRec, name: String, strict: Boolean): Val @cps[MachineOp] = envRec match {
    case envRec: DeclarativeEnvRec => {
      val bindings = @*(envRec.bindings)
      assert(bindings.contains(name))
      bindings(name) match {
        case mb: MutableBinding => {
          @*(mb.v)
        }
        case ib: ImmutableBinding => {
          val v = @*(ib.v)
          v match {
            case None => if (strict) moThrow("ReferenceError") else moVal(VUndef)
            case Some(v) => moVal(v)
          }
        }
      }
    }
    case envRec: ObjectEnvRec => {
      val bindings = envRec.bindingObj
      val bindingsData = @*(bindings.cell)
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
      val objData = @*(o.cell)
      objData.isInstanceOf[CallableObj]
    }
    case _ => false
  }

  def evaluateExpression(expr: Expression): ValOrRef @cps[MachineOp] = expr match {
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
      val funcData = @*(func.asInstanceOf[VObj].cell)
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
  def newFunctionObj(params: List[String], body: List[SourceElement], lexScope: LexEnv, strict: Boolean): VObj @cps[MachineOp] = {
//    Create a new native ECMAScript object and let F be that object.
    val f = VObj(alloc[Cell[ObjData]])
    val fProps = @<(Map.empty[PropName, Prop])
    val fData = new BaseObj with CallableObj {
      def thisVal: VObj = f
      val props: Cell[Map[PropName, Prop]] = fProps

//    Set all the internal methods, except for [[Get]], of F as described in 8.12.
//
//    Set the [[Class]] internal property of F to "Function".
      override def clazz = "Function"
//    Set the [[Prototype]] internal property of F to the standard built-in Function prototype object as specified in 15.3.3.1.
      def prototype: Option[VObj] = None // FIXME: Stub
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
        moComplete(evaluateSourceElements(code.ses))
        // FIXME: Proper completion handling
      }
//    Set the [[Construct]] internal property of F as described in 13.2.2.
//    Set the [[HasInstance]] internal property of F as described in 15.3.5.3.
//    Set the [[Scope]] internal property of F to the value of Scope.
      override def scope = lexScope
//    Let names be a List containing, in left to right textual order, the Strings corresponding to the identifiers of FormalParameterList.
//    Set the [[FormalParameters]] internal property of F to names.
      override def formalParameters = params
//    Set the [[Code]] internal property of F to FunctionBody.
      override def code = FunctionCode(f, body, strict)
//    Set the [[Extensible]] internal property of F to true.
      override def extensible = true
    }
    f.cell @= fData
//
//    Let len be the number of formal parameters specified in FormalParameterList. If no parameters are specified, let len be 0.
//
//    Call the [[DefineOwnProperty]] internal method of F with arguments "length", Property Descriptor {[[Value]]: len, [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false}, and false.
//
//    Let proto be the result of creating a new object as would be constructed by the expression new Object()where Object is the standard built-in constructor with that name.
//
//    Call the [[DefineOwnProperty]] internal method of proto with arguments "constructor", Property Descriptor {[[Value]]: F, { [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true}, and false.
//
//    Call the [[DefineOwnProperty]] internal method of F with arguments "prototype", Property Descriptor {[[Value]]: proto, { [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false}, and false.
//
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
          moVal(Ref(envRec, name, strict))
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
      case FunctionCode(func, _, _) => {
        val names = Nil.asInstanceOf[List[String]] //getInternalProperty(func, "[[FormalParameters]]")
        val argCount = args.length
        cpsIterable(names).cps.foreach { (argName: String) =>
          moVal((): Any)
        }
        var n = 0 // Manually increment because zipWithIndex didn't play with cps
        for (argName <- names.cps) {
          n += 1
          val v = if (n > argCount) VUndef else args(n)
          val argAlreadyDeclared = env.hasBinding(argName)
          if (!argAlreadyDeclared) env.createMutableBinding(argName, false) else moNop // FIXME: Spec doesn't specify value of canDelete
          env.setMutableBinding(argName, v, strict)
        }
      }
      case _ => moVal(())
    }
    cpsIterable(cxt.code.ses).cps.foreach {
      case FunctionDeclarationSourceElement(fd@FunctionDeclaration(fn, _, _)) => {
        val fo = newFunctionObjFromDeclaration(fd, cxt.lexEnv, strict) // FIXME: Instantiate function
        val funcAlreadyDeclared = env.hasBinding(fn)
        if (!funcAlreadyDeclared) {
          env.createMutableBinding(fn, configurableBindings)
        } else if (!cxt.varEnv.outer.isDefined) {
          val go = @*(getGlobalObj.cell)
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
          } else moNop
        } else moNop
        env.setMutableBinding(fn, fo, strict)
      }
      case _ => ()
    }
    ()
  }

  def interpret(programSource: String): Completion = {
    val p = Parser.parse(programSource)

    val strictMode = hasUseStrictDirective(p.ses)
    if (p.ses.isEmpty) return Completion(CNormal, None, None) // Optimization from spec

    val globalCode = GlobalCode(p.ses)
    val h1 = new Heap()
    val (h2, globalObjCell) = h1.alloc[ObjData]
    val globalObj = VObj(globalObjCell)
    val globalEnv = LexEnv(ObjectEnvRec(globalObj), None)
    val progCxt = ExecContext(globalEnv, globalEnv, globalObj, globalCode)
    val m = Machine(progCxt, h2, globalObj, globalEnv)

    val (m1, c) = runMachineOp(m, reset {
      globalObjCell @= new NativeObj(globalObj, @<(Map.empty), None)
      instantiateDeclarationBindings(Nil)
      val evalCompletion = evaluateSourceElements(p.ses)
      MOComp((_: Machine) => evalCompletion)
    })
    c
  }

}