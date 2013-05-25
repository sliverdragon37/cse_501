import scala.collection.mutable._

sealed trait Operand {

}
sealed trait IRType {

}

sealed trait LocalRep {
  val s:String
  val n:Option[Int]
  override def toString = {
    val num = n match {
      case Some(n) => n.toString
      case None => "?"
    }
    s + "#" + num
  }
}

sealed trait PrimitiveType extends IRType {
}
sealed trait BoxedType extends IRType {
}
sealed trait NumberT {
}
sealed trait BooleanT {
}

sealed trait SIR {
  var live = true
  def opMap(f:(Operand => Operand)):Unit = {
  }
}
sealed trait Instr {
  //instruction number
  var num:Int = -1
  def repr:String
  override def toString = "    instr " + num.toString + ": " + repr
}

sealed trait SSA extends SIR {

}

sealed trait Op extends SIR {
  var a:Operand
  //def unapply(x:Operand):Option[Operand] = Some(a)
  override def opMap(f:(Operand => Operand)):Unit = a = f(a)
}
sealed trait Opop extends SIR {
  var a:Operand
  var b:Operand
  //def unapply(x:Operand,y:Operand):Option[(Operand,Operand)]
  override def opMap(f:(Operand => Operand)):Unit = {
    a = f(a)
    b = f(b)
  }
}
sealed trait Opt extends SIR {
  var a:Operand
  val t:IRType
  //def unapply(x:Operand,z:IRType):Option[(Operand,IRType)] = Some(a,t)
  override def opMap(f:(Operand => Operand)):Unit = a = f(a)
}
sealed trait Opopt extends SIR {
  var a:Operand
  var b:Operand
  val t:IRType
  //def unapply(x:Operand,y:Operand,z:IRType):Option[(Operand,Operand,IRType)] = Some(a,b,t)
  override def opMap(f:(Operand => Operand)):Unit = {
    a = f(a)
    b = f(b)
  }
}
sealed trait Opopop extends SIR {
  var a:Operand
  var b:Operand
  var c:Option[Operand]
  //def unapply(x:Operand,y:Operand,z:Option[Operand]):Option[(Operand,Operand,Option[Operand])] = Some(a,b,c)
  override def opMap(f:(Operand => Operand)):Unit = {
    a = f(a)
    b = f(b)
    c match {
      case None =>
      case Some(x) => c = Some(f(x))
    }
  }
}

sealed trait Declaration {
  def argToS(v:(String,Int,IRType)):String = v._1 + "#" + v._2 + ":" + v._3 
  def argsToS(lv:List[(String,Int,IRType)]):String = lv.map(argToS(_) + " ").foldLeft("")(_+_)
}

case object ListType extends BoxedType { override def toString = "List" }
case object IntegerType extends BoxedType with NumberT { override def toString = "Integer" }
case object BooleanType extends BoxedType with BooleanT { override def toString = "Boolean" }
case class UserType(s:String) extends BoxedType { override def toString = s }
case class PointerType(t:IRType) extends PrimitiveType { override def toString = t.toString + "*" }
case object DynamicType extends BoxedType { override def toString = "dynamic" }
case object IntType extends PrimitiveType with NumberT { override def toString = "int" }
case object BoolType extends PrimitiveType with BooleanT { override def toString = "bool" }

case class Register(n:Int) extends Operand { override def toString = "(" + n + ")" }
case class Immediate(n:Int) extends ValNum { override def toString = n.toString }
case class Base(s:String,n:Option[Int]) extends Operand with LocalRep
case class Offset(s:String,n:Option[Int]) extends Operand with LocalRep
case class TypeOper(s:String,n:Option[Int]) extends Operand with LocalRep
case class ParamOper(s:String,n:Option[Int]) extends Operand with LocalRep 


case class Local(s:String,n:Option[Int]) extends Operand with LocalRep {
  private var counter = 0
  private val stack = new Stack[SSALocal]()
  def genName:SSALocal = {
    val l = SSALocalVar(this,counter)
    stack.push(l)
    counter += 1
    l
  }
  def getCurr = {
    if (stack.isEmpty) {
      DEAD
    } else {
      stack.head
    }
  }
  def pop = {
    stack.pop
  }
}

sealed trait SSALocal extends Operand
case object DEAD extends SSALocal
case class SSALocalVar(parent:Local,n:Int) extends SSALocal { 
  override def toString = parent.s + "$" + n
}
case class Location(n:Int) extends Operand { override def toString = "[" + n + "]" }
case object GlobalPointer extends Operand { override def toString = "GP" }
case object FramePointer extends Operand { override def toString = "FP" }
case class Dest(b:Block) extends Operand { override def toString = "[" + b.firstInstrLocation + "]" }
//vv only used in value numbering vv
trait ValNum extends Operand
case class ValNumber(parent:Operand) extends ValNum { override def toString = parent.toString }

//IR header info
case class TypeDeclaration(name:String,args:List[(String,Int,IRType)]) extends Declaration { override def toString = "    type " + name + ": " + argsToS(args) }
case class MethodDeclaration(name:String,start:Int,paramsAndLocals:List[(String,Int,IRType)]) extends Declaration { override def toString = "    method " + name + "@" + start + ": " + argsToS(paramsAndLocals) }
case class GlobalDeclaration(v:(String,Int,IRType)) extends Declaration { override def toString = "    global " + argToS(v) }

//program and method entry
case class Enter(var a:Operand) extends Op with Instr with SSA { def repr = "enter " + a }
case object Entrypc extends SIR with Instr with SSA { def repr = "entrypc" }

//branch instructions
case class Br(var a:Operand) extends Op with Instr with SSA { def repr = "br " + a }
case class Blbc(var a:Operand,var b:Operand,var c:Option[Operand]) extends Opopop with Instr with SSA { def repr = "blbc " + a + " " + b }
case class Blbs(var a:Operand,var b:Operand,var c:Option[Operand]) extends Opopop with Instr with SSA { def repr = "blbs " + a + " " + b }
case class Call(var a:Operand) extends Op with Instr with SSA { def repr = "call " + a }
case class Ret(var a:Operand) extends Op with Instr with SSA { def repr = "ret " + a }
case class Nop() extends SIR with Instr with SSA { def repr = "nop" }

case class Add(var a:Operand,var b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "add " + a + " " + b + " :" + t }
case class Sub(var a:Operand,var b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "sub " + a + " " + b + " :" + t }
case class Mul(var a:Operand,var b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "mul " + a + " " + b + " :" + t }
case class Div(var a:Operand,var b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "div " + a + " " + b + " :" + t }
case class Mod(var a:Operand,var b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "mod " + a + " " + b + " :" + t }
case class Neg(var a:Operand,t:IRType) extends Opt with Instr with SSA { def repr = "neg " + a + " :" + t }
case class Cmpeq(var a:Operand,var b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "cmpeq " + a + " " + b + " :" + t }
case class Cmple(var a:Operand,var b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "cmple " + a + " " + b + " :" + t }
case class Cmplt(var a:Operand,var b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "cmplt " + a + " " + b + " :" + t }
case class Isnull(var a:Operand,t:IRType) extends Opt with Instr with SSA { def repr = "isnull " + a + " :" + t }
case class Istype(var a:Operand,var b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "istype " + a + " " + b + " :" + t }
case class Load(var a:Operand,t:IRType) extends Opt with Instr with SSA { def repr = "load " + a + " :" + t}
case class Store(var a:Operand,var b:Operand) extends Opop with Instr with SSA { def repr = "store " + a + " " + b}
case class Move(var a:Operand,var b:Operand) extends Opop with Instr with SSA { def repr = "move " + a + " " + b}
case class New(var a:Operand,t:IRType) extends Opt with Instr with SSA { def repr = "new " + a + " :" + t}
case class Newlist(var a:Operand,t:IRType) extends Opt with Instr with SSA { def repr = "newlist " + a + " :" + t}
case class Checknull(var a:Operand,t:IRType) extends Opt with Instr with SSA { def repr = "checknull " + a + " :" + t }
case class Checktype(var a:Operand,var b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "checktype " + a + " " + b + " :" + t }
case class Checkbounds(var a:Operand,var b:Operand) extends Opop with Instr with SSA { def repr = "checkbounds " + a + " " + b }
case class Lddynamic(var a:Operand,var b:Operand,t:IRType) extends Opopt with Instr with SSA { def repr = "lddynamic " + a + " " + b + " :" + t }
case class Stdynamic(var a:Operand,var b:Operand,var c:Option[Operand]) extends Opopop with Instr with SSA { def repr = "stdynamic " + a + " " + b + " " + (c match { 
  case Some(x) => x
  case None => throw new RuntimeException("this will never happen")
})}
case class Write(var a:Operand) extends Op with Instr with SSA { def repr = "write " + a }
case class Wrl() extends SIR with Instr with SSA { def repr = "wrl" }
case class Param(var a:Operand) extends Op with Instr with SSA { def repr = "param " + a }

//instrumentation instruction
//todo: put support in parser for this
case class Count(var a:Operand) extends Op with Instr with SSA { def repr = "count " + a }

//phi functions
case class Phi(a:Local) extends SSA with Instr {
  val args = new HashMap[Block,SSALocal]()
  def add(b:Block,s:SSALocal) {
    args += (b->s)
  }
  def repr = {
    val s = if (ssa != null) { ssa } else { a }
    "phi " + s + " <- " + args
  }
  var ssa:SSALocal = null
  def gen = ssa = a.genName
}

//an entire method definition
case class Method(instrs:List[SIR with Instr],name:String,args:List[IRType],locals:List[IRType])



