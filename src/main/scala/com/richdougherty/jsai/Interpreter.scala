package com.richdougherty.jsai

import Parser._

class Interpreter {

  sealed trait Val
  case object VUndef extends Val
  case object VNull extends Val
  case class VBool(d: Boolean) extends Val
  case class VStr(d: String) extends Val
  case class VNum(d: Double) extends Val
  case class VObj(d: ObjRef) extends Val

  type ObjRef = Int
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

  case class Machine(heap: Map[ObjRef, ObjData])
  case class ExecutionContext(varEnv: LexicalEnvironment, lexEnv: LexicalEnvironment, thisBinding: ObjRef)

  case class LexicalEnvironment(er: EnvironmentRecord, outer: Option[EnvironmentRecord])

  sealed trait EnvironmentRecord
  case class DeclarativeEnvironmentRecord(bindings: Map[String, Binding]) extends EnvironmentRecord
  case class ObjectEnvironmentRecord(bindingObj: ObjRef, provideThis: Boolean = false) extends EnvironmentRecord

  sealed trait Binding
  case class MutableBinding(v: Option[Val], canDelete: Boolean) extends Binding
  case class ImmutableBinding(v: Option[Val]) extends Binding

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

  def evaluateSourceElements(cxt: ExecutionContext, ses: List[SourceElement]): Val = {
    VUndef
  }

  def interpret(programSource: String): Val = {
    val p = Parser.parse(programSource)
    val globalRef = 1 // Stubbed ObjRef to global object
    val globalEnv = LexicalEnvironment(ObjectEnvironmentRecord(globalRef), None)

    val strictMode = hasUseStrictDirective(p.ses)
    if (p.ses.isEmpty) return VUndef // (normal, empty, empty)
    val progCxt = ExecutionContext(globalEnv, globalEnv, globalRef)
    val result = evaluateSourceElements(progCxt, p.ses)
    // return result
    VUndef
  }

}