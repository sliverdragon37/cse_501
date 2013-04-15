
trait Operand {
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
case class UserType(s:String) extends BoxedType
case class PointerType(t:IRType) extends PrimitiveType
case object DynamicType extends BoxedType
case object IntType extends PrimitiveType with NumberT
case object BoolType extends PrimitiveType with BooleanT



case class Register(n:Int) extends Operand
case class Immediate(n:Int) extends Operand
case class Local(s:String,n:Int) extends Operand
case class Location(n:Int) extends Operand
case object GlobalPointer extends Operand
case object FramePointer extends Operand

trait SIR {
  var num:Int = -1
}

trait Declaration {

}

//IR header info
case class TypeDeclaration(name:String,args:List[(String,Int,IRType)]) extends Declaration
case class MethodDeclaration(name:String,start:Int,paramsAndLocals:List[(String,Int,IRType)]) extends Declaration
case class GlobalDeclaration(v:(String,Int,IRType)) extends Declaration

//program and method entry
case class Enter(bytes:Operand) extends SIR
case object EntryPC extends SIR

//branch instructions
case class Br(dest:Operand) extends SIR
case class Blbc(reg:Operand,dest:Operand) extends SIR
case class Blbs(reg:Operand,dest:Operand) extends SIR
case class Call(fun:Operand) extends SIR
case class Ret(bytes:Operand) extends SIR
case object Nop extends SIR
case class Other(s:List[String]) extends SIR


case class Method(instrs:List[SIR],name:String,args:List[IRType],locals:List[IRType])




