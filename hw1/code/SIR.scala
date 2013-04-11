
//trait shared by 
trait SIR {

}

trait Param {

}

trait IRType {

}

trait PrimitiveType extends IRType
trait BoxedType extends IRType
trait NumberT
trait BooleanT

case object ListType extends BoxedType
case object IntegerType extends BoxedType with NumberT
case object BooleanType extends BoxedType with BooleanT
case class PointerType(t:IRType) extends PrimitiveType
case object DynamicType extends BoxedType
case object IntType extends PrimitiveType with NumberT
case object BoolType extends PrimitiveType with BooleanT


case class Register(n:Int) extends Param
case class Immediate(n:Int) extends Param

case class Enter(bytes:Param) extends SIR
case object EntryPC extends SIR
//...


case class Method(instrs:List[SIR],name:String,args:List[IRType],locals:List[IRType])



