import scala.util.parsing.combinator.syntactical._
import scala.collection.mutable._

object SIRParser extends StandardTokenParsers {

  val instrs = List("br","blbs","blbc","call","ret","nop","add","sub","mul","div","mod","neg","cmpeq","cmple","cmplt","isnull","istype","load","store","move","new","newlist","checknull","checktype","checkbounds","lddynamic","stdynamic","write","wrl","enter","param","entrypc")
  val keywords = List("type","method","global","instr")
  val types = List("List","Integer","Boolean","dynamic","int","bool")
  val regs = List("GP","FP")

  var symbolTable = new HashMap[String,Local]()

  lexical.reserved ++= instrs
  lexical.reserved ++= keywords
  lexical.reserved ++= types
  lexical.reserved ++= regs

  lexical.delimiters ++= List(":","[","]","#","(",")","*","@","-","_","?")

  type Arg = (String,Int,IRType)

  def numLit:Parser[Int] = opt("-") ~ numericLit ^^ {
    case None ~ n => n.toInt
    case Some(_) ~ n => (-1)*n.toInt
  }

  def opcodes = rep(opcode)

  def opcode:Parser[Either[SIR with Instr,Declaration]] = 
    ( header ^^ { case h => Right(h) }) |
    (instruction ^^ { case i => Left(i) })
  def header:Parser[Declaration] = typHeader | methodHeader | globalHeader
  def typHeader:Parser[Declaration] = ("type" ~> ident ~ ":" ~ arglist) ^^ {case cname ~ _ ~ args => TypeDeclaration(cname,args) }
  def methodHeader:Parser[Declaration] = ("method" ~> ident ~ "@" ~ numLit ~ ":" ~ arglist) ^^ { case mname ~ _ ~ codeAddr ~ _ ~ args => MethodDeclaration(mname,codeAddr,args) }
  def globalHeader:Parser[Declaration] = ("global" ~> arg) ^^ {case a => GlobalDeclaration(a)}

  def arglist = rep(arg)
  def arg:Parser[Arg] = ident ~ "#" ~ numLit ~ ":" ~ typ ^^ {case s ~ _ ~ n ~ _ ~ t => (s,n,t)}

  def instruction:Parser[SIR with Instr] = "instr" ~> numLit ~ ":" ~ op ^^ { case n ~ _ ~ op => {
    op.num = n
    op
  }}

  def oparg:Parser[Operand] = reg | imm | loc | gp | fp | local
  def reg = "(" ~> numLit <~ ")" ^^ { case n => Register(n) }
  def imm = numLit ^^ { case n => Immediate(n) }
  def local = (
    (ident ~ "#" ~ (numLit ^^ {case n => Some(n)} | "?" ^^ {case _ => None})) 
      ^^ { case s ~ _ ~ n => {
        if (s.endsWith("_base")) {
          Base(s,n)
        } else if (s.endsWith("_offset")) {
          Offset(s,n)
        } else if (s.endsWith("_type")) {
          TypeOper(s,n)
        } else {
          symbolTable.get(s) match {
            case Some(l) => l
            case None => {
              val l = Local(s,n)
              symbolTable += (s->l)
              l
            }
          }
        }
      }
      }
  )
  def loc = "[" ~> numLit <~ "]" ^^ { case n => Location(n) }
  def gp = "GP" ^^ { case _ => GlobalPointer }
  def fp = "FP" ^^ { case _ => FramePointer }

  //Main parser for SIR ops
  def op:Parser[SIR with Instr] = (
    ("br" ~> loc ^^ {case l => Br(l)}) |
      ("blbc" ~> oparg ~ loc ^^ {case o ~ l => Blbc(o,l,None)}) |
      ("blbs" ~> oparg ~ loc ^^ {case o ~ l => Blbs(o,l,None)}) |
      ("call" ~> loc ^^ {case l => Call(l)}) |
      ("nop" ^^ {case _ => Nop()}) |
      ("add" ~> oparg ~ oparg ~ ":" ~ typ ^^ {case a1 ~ a2 ~ _ ~ t => Add(a1,a2,t) }) |
      ("sub" ~> oparg ~ oparg ~ ":" ~ typ ^^ {case a1 ~ a2 ~ _ ~ t => Sub(a1,a2,t) }) |
      ("mul" ~> oparg ~ oparg ~ ":" ~ typ ^^ {case a1 ~ a2 ~ _ ~ t => Mul(a1,a2,t) }) |
      ("div" ~> oparg ~ oparg ~ ":" ~ typ ^^ {case a1 ~ a2 ~ _ ~ t => Div(a1,a2,t) }) |
      ("mod" ~> oparg ~ oparg ~ ":" ~ typ ^^ {case a1 ~ a2 ~ _ ~ t => Mod(a1,a2,t) }) |
      ("neg" ~> oparg ~ ":" ~ typ ^^ {case a ~ _ ~ t => Neg(a,t) }) |
      ("cmpeq" ~> oparg ~ oparg ~ ":" ~ typ ^^ {case a1 ~ a2 ~ _ ~ t => Cmpeq(a1,a2,t) }) |
      ("cmple" ~> oparg ~ oparg ~ ":" ~ typ ^^ {case a1 ~ a2 ~ _ ~ t => Cmple(a1,a2,t) }) |
      ("cmplt" ~> oparg ~ oparg ~ ":" ~ typ ^^ {case a1 ~ a2 ~ _ ~ t => Cmplt(a1,a2,t) }) |
      ("isnull" ~> oparg ~ ":" ~ typ ^^ {case a ~ _ ~ t => Isnull(a,t) }) |
      ("istype" ~> oparg ~ oparg ~ ":" ~ typ ^^ {case a ~ t ~ _ ~ rt => Istype(a,t,rt) }) |
      ("load" ~> oparg ~ ":" ~ typ ^^ {case a ~ _ ~ t => Load(a,t) }) |
      ("store" ~> oparg ~ oparg ^^ {case a1 ~ a2 => Store(a1,a2) }) |
      ("move" ~> oparg ~ oparg ^^ {case a1 ~ a2 => Move(a1,a2) }) |
      ("new" ~> oparg ~ ":" ~ typ ^^ {case a ~ _ ~ t => New(a,t) }) |
      ("newlist" ~> oparg ~ ":" ~ typ ^^ {case a ~ _ ~ t => Newlist(a,t) }) |
      ("checknull" ~> oparg ~ ":" ~ typ ^^ {case a ~ _ ~ t => Checknull(a,t) }) |
      ("checktype" ~> oparg ~ oparg ~ ":" ~ typ ^^ {case a1 ~ a2 ~ _ ~ t => Checktype(a1,a2,t) }) |
      ("checkbounds" ~> oparg ~ oparg ^^ {case a1 ~ a2 => Checkbounds(a1,a2) }) |
      ("lddynamic" ~> oparg ~ oparg ~ ":" ~ typ ^^ {case a1 ~ a2 ~ _ ~ t => Lddynamic(a1,a2,t) }) |
      ("stdynamic" ~> oparg ~ oparg ^^ {case a1 ~ a2 => Stdynamic(a1,a2) }) |
      ("write" ~> oparg ^^ {case a => Write(a) }) |
      ("wrl" ^^ {case _ => Wrl() }) |
      ("enter" ~> imm ^^ {case n => Enter(n)}) |
      ("ret" ~> imm ^^ {case n => Ret(n)}) |
      ("param" ~> oparg ^^ {case o => Param(o)}) |
      ("entrypc" ^^ {case _ => Entrypc})
      // | ((ident ~ rep(oparg ^^ {case o => o.toString}) ~ opt(":" ~ typ ^^{case a ~ b => a+b.toString}))
      // ^^ {
      //   case s ~ ls ~ Some(sl) => Other(List(s)++ls++List(sl))
      //   case s ~ ls ~ None => Other(List(s)++ls)
      // })
  )

  def typ:Parser[IRType] = (baseType ~ ptr) ^^ {case b ~ n =>
      { var r = b
        (0 until n).foreach({_ => r = PointerType(r)})
        r
      }}

  def baseType:Parser[IRType] = (
    ("List" ^^ {case _ => ListType}) |
    ("Integer" ^^ {case _ => IntegerType}) |
    ("Boolean" ^^ {case _ => BooleanType}) |
    ("dynamic" ^^ {case _ => DynamicType}) |
    ("int" ^^ {case _ => IntType}) |
    ("bool" ^^ {case _ => BoolType}) |
    (ident ^^ {case s => UserType(s)})
  )

  def ptr:Parser[Int] = rep("*") ^^ {case l => l.length}

  def runParser(in:String):Either[SIR with Instr,Declaration] = {
    val tokens = new lexical.Scanner(in)
    val p = phrase(opcode)(tokens)
    p match {
      case Success(r,_) => {
        //println(in)
        //println(r)
        r
      }
      case _ => throw new RuntimeException("bad parse on input: " + in)
    }
  }
}
