
trait Operand {
}

trait IRType {

}

trait PrimitiveType extends IRType
trait BoxedType extends IRType
trait NumberT
trait BooleanT

case object ListType extends BoxedType { override def toString = "List" }
case object IntegerType extends BoxedType with NumberT { override def toString = "Int" }
case object BooleanType extends BoxedType with BooleanT { override def toString = "Boolean" }
case class UserType(s:String) extends BoxedType { override def toString = s }
case class PointerType(t:IRType) extends PrimitiveType { override def toString = t.toString + "*" }
case object DynamicType extends BoxedType { override def toString = "dynamic" }
case object IntType extends PrimitiveType with NumberT { override def toString = "int" }
case object BoolType extends PrimitiveType with BooleanT { override def toString = "bool" }

case class Register(n:Int) extends Operand { override def toString = "(" + n + ")" }
case class Immediate(n:Int) extends Operand { override def toString = n.toString }
case class Local(s:String,n:Option[Int]) extends Operand { 
  override def toString = {
    val num = n match {
      case Some(n) => n.toString
      case None => "?"
    }
    s + "#" + num
  }
}
case class Location(n:Int) extends Operand { override def toString = "[" + n + "]" }
case object GlobalPointer extends Operand { override def toString = "GP" }
case object FramePointer extends Operand { override def toString = "FP" }

trait SIR {

}

trait Instr {
  //instruction number
  var num:Int = -1
  def repr:String
  override def toString = "instr " + num.toString + ": " + repr
}

trait Declaration {

}

//IR header info
case class TypeDeclaration(name:String,args:List[(String,Int,IRType)]) extends Declaration
case class MethodDeclaration(name:String,start:Int,paramsAndLocals:List[(String,Int,IRType)]) extends Declaration
case class GlobalDeclaration(v:(String,Int,IRType)) extends Declaration

//program and method entry
case class Enter(bytes:Operand) extends SIR with Instr { def repr = "enter " + bytes }
case object Entrypc extends SIR with Instr { def repr = "entrypc" }

//branch instructions
case class Br(dest:Operand) extends SIR with Instr { def repr = "br " + dest }
case class Blbc(reg:Operand,dest:Operand) extends SIR with Instr { def repr = "blbc " + reg + " " + dest }
case class Blbs(reg:Operand,dest:Operand) extends SIR with Instr { def repr = "blbs " + reg + " " + dest }
case class Call(fun:Operand) extends SIR with Instr { def repr = "call " + fun }
case class Ret(bytes:Operand) extends SIR with Instr { def repr = "ret " + bytes }
case object Nop extends SIR with Instr { def repr = "nop" }

case class Add(a:Operand,b:Operand,t:IRType) extends SIR with Instr { def repr = "add " + a + " " + b + " :" + t }
case class Sub(a:Operand,b:Operand,t:IRType) extends SIR with Instr { def repr = "sub " + a + " " + b + " :" + t }
case class Mul(a:Operand,b:Operand,t:IRType) extends SIR with Instr { def repr = "mul " + a + " " + b + " :" + t }
case class Div(a:Operand,b:Operand,t:IRType) extends SIR with Instr { def repr = "div " + a + " " + b + " :" + t }
case class Mod(a:Operand,b:Operand,t:IRType) extends SIR with Instr { def repr = "mod " + a + " " + b + " :" + t }
case class Neg(a:Operand,t:IRType) extends SIR with Instr { def repr = "neg " + a + " :" + t }
case class Cmpeq(a:Operand,b:Operand,t:IRType) extends SIR with Instr { def repr = "cmpeq " + a + " " + b + " :" + t }
case class Cmple(a:Operand,b:Operand,t:IRType) extends SIR with Instr { def repr = "cmple " + a + " " + b + " :" + t }
case class Cmplt(a:Operand,b:Operand,t:IRType) extends SIR with Instr { def repr = "cmplt " + a + " " + b + " :" + t }
case class Isnull(a:Operand,t:IRType) extends SIR with Instr { def repr = "isnull " + a + " :" + t }
case class Istype(a:Operand,b:Operand,t:IRType) extends SIR with Instr { def repr = "istype " + a + " " + b + " :" + t }
case class Load(a:Operand,t:IRType) extends SIR with Instr { def repr = "load " + a + " :" + t}
case class Store(a:Operand,b:Operand) extends SIR with Instr { def repr = "store " + a + " " + b}
case class Move(a:Operand,b:Operand) extends SIR with Instr { def repr = "move " + a + " " + b}
case class New(a:Operand,t:IRType) extends SIR with Instr { def repr = "new " + a + " :" + t}
case class Newlist(a:Operand,t:IRType) extends SIR with Instr { def repr = "newlist " + a + " :" + t}
case class Checknull(a:Operand,t:IRType) extends SIR with Instr { def repr = "checknull " + a + " :" + t }
case class Checktype(a:Operand,b:Operand,t:IRType) extends SIR with Instr { def repr = "checktype " + a + " " + b + " :" + t }
case class Checkbounds(a:Operand,b:Operand) extends SIR with Instr { def repr = "checkbounds " + a + " " + b }
case class Lddynamic(a:Operand,b:Operand,t:IRType) extends SIR with Instr { def repr = "lddynamic " + a + " " + b + " :" + t }
case class Stdynamic(a:Operand,b:Operand) extends SIR with Instr { def repr = "stdynamic " + a + " " + b }
case class Write(a:Operand) extends SIR with Instr { def repr = "write " + a }
case object Wrl extends SIR with Instr { def repr = "wrl" }
case class Param(a:Operand) extends SIR with Instr { def repr = "param " + a }


//case class Other(s:List[String]) extends SIR with Instr { def repr = s.foldLeft("")((a,b) => a + b)}


case class Method(instrs:List[SIR],name:String,args:List[IRType],locals:List[IRType])




