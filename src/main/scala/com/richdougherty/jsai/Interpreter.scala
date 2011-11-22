package com.richdougherty.jsai



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

  def interpret(programSource: String): Val = {
    val parser = new Parser()
    val program = parser.parse(programSource)
    VUndef
  }

}