
sealed trait Operand {
}

sealed trait IRType {

}

sealed trait PrimitiveType extends IRType
sealed trait BoxedType extends IRType
sealed trait NumberT
sealed trait BooleanT

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
  private var counter = 0
  def getNext:Int = {
    counter += 1
    counter
  }
  def getCurrent:Int = counter
}
case class SSALocal(s:String,parent:Local) extends Operand { override def toString = s }
case class Location(n:Int) extends Operand { override def toString = "[" + n + "]" }
case object GlobalPointer extends Operand { override def toString = "GP" }
case object FramePointer extends Operand { override def toString = "FP" }

sealed trait SIR {

}

sealed trait Instr {
  //instruction number
  var num:Int = -1
  def repr:String
  override def toString = "    instr " + num.toString + ": " + repr
}

sealed trait SSA {

}

sealed trait Op extends SIR {
  val a:Operand
  def unapply(x:Operand):Option[Operand] = Some(a)
}
sealed trait Opop extends SIR {
  val a:Operand
  val b:Operand
  def unapply(x:Operand,y:Operand):Option[(Operand,Operand)] = Some(a,b)
}
sealed trait Opt extends SIR {
  val a:Operand
  val t:IRType
  def unapply(x:Operand,z:IRType):Option[(Operand,IRType)] = Some(a,t)
}
sealed trait Opopt extends SIR {
  val a:Operand
  val b:Operand
  val t:IRType
  def unapply(x:Operand,y:Operand,z:IRType):Option[(Operand,Operand,IRType)] = Some(a,b,t)
}

sealed trait Declaration {
  def argToS(v:(String,Int,IRType)):String = v._1 + "#" + v._2 + ":" + v._3 
  def argsToS(lv:List[(String,Int,IRType)]):String = lv.map(argToS(_) + " ").foldLeft("")(_+_)
}

//IR header info
case class TypeDeclaration(name:String,args:List[(String,Int,IRType)]) extends Declaration { override def toString = "    type " + name + ": " + argsToS(args) }
case class MethodDeclaration(name:String,start:Int,paramsAndLocals:List[(String,Int,IRType)]) extends Declaration { override def toString = "    method " + name + "@" + start + ": " + argsToS(paramsAndLocals) }
case class GlobalDeclaration(v:(String,Int,IRType)) extends Declaration { override def toString = "    global " + argToS(v) }

//program and method entry
case class Enter(a:Operand) extends Op with Instr with SSA { def repr = "enter " + a }
case object Entrypc extends SIR with Instr with SSA { def repr = "entrypc" }

//branch instructions
case class Br(a:Operand) extends Op with Instr with SSA { def repr = "br " + a }
case class Blbc(a:Operand,b:Operand) extends Opop with Instr with SSA { def repr = "blbc " + a + " " + b }
case class Blbs(a:Operand,b:Operand) extends Opop with Instr with SSA { def repr = "blbs " + a + " " + b }
case class Call(a:Operand) extends Op with Instr with SSA { def repr = "call " + a }
case class Ret(a:Operand) extends Op with Instr with SSA { def repr = "ret " + a }
case class Nop() extends SIR with Instr with SSA { def repr = "nop" }

case class Add(a:Operand,b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "add " + a + " " + b + " :" + t }
case class Sub(a:Operand,b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "sub " + a + " " + b + " :" + t }
case class Mul(a:Operand,b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "mul " + a + " " + b + " :" + t }
case class Div(a:Operand,b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "div " + a + " " + b + " :" + t }
case class Mod(a:Operand,b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "mod " + a + " " + b + " :" + t }
case class Neg(a:Operand,t:IRType) extends Opt with Instr with SSA { def repr = "neg " + a + " :" + t }
case class Cmpeq(a:Operand,b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "cmpeq " + a + " " + b + " :" + t }
case class Cmple(a:Operand,b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "cmple " + a + " " + b + " :" + t }
case class Cmplt(a:Operand,b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "cmplt " + a + " " + b + " :" + t }
case class Isnull(a:Operand,t:IRType) extends Opt with Instr with SSA { def repr = "isnull " + a + " :" + t }
case class Istype(a:Operand,b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "istype " + a + " " + b + " :" + t }
case class Load(a:Operand,t:IRType) extends Opt with Instr with SSA { def repr = "load " + a + " :" + t}
case class Store(a:Operand,b:Operand) extends Opop with Instr with SSA { def repr = "store " + a + " " + b}
case class Move(a:Operand,b:Operand) extends Opop with Instr with SSA { def repr = "move " + a + " " + b}
case class New(a:Operand,t:IRType) extends Opt with Instr with SSA { def repr = "new " + a + " :" + t}
case class Newlist(a:Operand,t:IRType) extends Opt with Instr with SSA { def repr = "newlist " + a + " :" + t}
case class Checknull(a:Operand,t:IRType) extends Opt with Instr with SSA { def repr = "checknull " + a + " :" + t }
case class Checktype(a:Operand,b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "checktype " + a + " " + b + " :" + t }
case class Checkbounds(a:Operand,b:Operand) extends Opop with Instr with SSA { def repr = "checkbounds " + a + " " + b }
case class Lddynamic(a:Operand,b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "lddynamic " + a + " " + b + " :" + t }
case class Stdynamic(a:Operand,b:Operand) extends Opop with Instr with SSA { def repr = "stdynamic " + a + " " + b }
case class Write(a:Operand) extends Op with Instr with SSA { def repr = "write " + a }
case class Wrl() extends SIR with Instr with SSA { def repr = "wrl" }
case class Param(a:Operand) extends Op with Instr with SSA { def repr = "param " + a }


//case class Other(s:List[String]) extends SIR with Instr { def repr = s.foldLeft("")((a,b) => a + b)}


case class Method(instrs:List[SIR with Instr],name:String,args:List[IRType],locals:List[IRType])




