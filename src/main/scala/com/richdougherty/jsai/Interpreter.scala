package com.richdougherty.jsai

import Parser.AdditionOperator
import Parser.Expression
import Parser.ExpressionStatement
import Parser.InfixExpression
import Parser.LiteralExpression
import Parser.SourceElement
import Parser.StatementSourceElement
import Parser.StringLiteral

class Interpreter {

  sealed trait Val
  case object VUndef extends Val
  case object VNull extends Val
  case class VBool(d: Boolean) extends Val
  case class VStr(d: String) extends Val
  case class VNum(d: Double) extends Val
  case class VObj(d: ObjPtr) extends Val

  type ObjPtr = Int
  class ObjData(d: Map[PropName, Prop], ipm: InternalPropMap)

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

  case class InternalPropMap(d: List[InternalProp]) {
    def get[P <: InternalProp]: Option[P] = {
      d.find(_.isInstanceOf[P]).asInstanceOf[Option[P]]
    }
    def put[P <: InternalProp](ip: P): InternalPropMap = {
      InternalPropMap(ip :: (d.filter(!_.isInstanceOf[P])))
    }
  }

  case class Machine(heap: Map[ObjPtr, ObjData])
  case class ExecutionContext(varEnv: LexicalEnvironment, lexEnv: LexicalEnvironment, thisBinding: ObjPtr)

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
  case class CContinue extends CompletionType
  case class CReturn extends CompletionType
  case class CThrow extends CompletionType

  def getDirectivePrologue(ses: List[SourceElement]): List[String] = {
    // FIXME: Use quoted, unescaped string literals as required by spec.
    def seToSl(se: SourceElement): Option[String] = se match {
      case StatementSourceElement(st) => st match {
        case ExpressionStatement(ex) => ex match {
          case LiteralExpression(lit) => lit match {
            case StringLiteral(d) => Some(d)
            case _ => None
          }
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

  def evaluateExpression(expr: Expression): Val = expr match {
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
    case LiteralExpression(lit) => lit match {
      case StringLiteral(s) => VStr(s)
    }
  }

  def evaluateSourceElement(se: SourceElement): Completion = se match {
    case StatementSourceElement(st) => st match {
      case ExpressionStatement(expr) => {
        val exprRef = evaluateExpression(expr)
        Completion(CNormal, Some(getValue(exprRef)), None)
      }
    }
  }

  def evaluateSourceElements(ses: List[SourceElement]): Completion = {
    ses.foldLeft(Completion(CNormal, None, None)) {
      case (Completion(CNormal, prevVal, prevTarget), se) => {
        val currCompletion = evaluateSourceElement(se)
        if (currCompletion.v.isDefined) currCompletion else currCompletion.copy(v = prevVal)
      }
      case (abrupt, _) => abrupt
    }
  }

  def interpret(programSource: String): Completion = {
    val p = Parser.parse(programSource)
    val globalPtr = 1 // Stubbed ObjPtr to global object
    val globalEnv = LexicalEnvironment(ObjectEnvironmentRecord(globalPtr), None)

    val strictMode = hasUseStrictDirective(p.ses)
    if (p.ses.isEmpty) return Completion(CNormal, None, None) // Optimization from spec
    val progCxt = ExecutionContext(globalEnv, globalEnv, globalPtr)
    val result = evaluateSourceElements(p.ses)
    result
  }

}