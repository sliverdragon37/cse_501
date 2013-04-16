import scala.util.parsing.combinator.syntactical._

object SIRParser extends StandardTokenParsers {

  val instrs = List("br","blbs","blbc","call","ret","nop")
  val keywords = List("type","method","global","instr")
  val types = List("List","Integer","Boolean","dynamic","int","bool")
  val regs = List("GP","FP")

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

  def opcode:Parser[Either[SIR,Declaration]] = 
    ( header ^^ { case h => Right(h) }) |
    (instruction ^^ { case i => Left(i) })
  def header:Parser[Declaration] = typHeader | methodHeader | globalHeader
  def typHeader:Parser[Declaration] = ("type" ~> ident ~ ":" ~ arglist) ^^ {case cname ~ _ ~ args => TypeDeclaration(cname,args) }
  def methodHeader:Parser[Declaration] = ("method" ~> ident ~ "@" ~ numLit ~ ":" ~ arglist) ^^ { case mname ~ _ ~ codeAddr ~ _ ~ args => MethodDeclaration(mname,codeAddr,args) }
  def globalHeader:Parser[Declaration] = ("global" ~> arg) ^^ {case a => GlobalDeclaration(a)}

  def arglist = rep(arg)
  def arg:Parser[Arg] = ident ~ "#" ~ numLit ~ ":" ~ typ ^^ {case s ~ _ ~ n ~ _ ~ t => (s,n,t)}

  def instruction:Parser[SIR] = "instr" ~> numLit ~ ":" ~ op ^^ { case n ~ _ ~ op => {
    op.num = n
    op
  }}

  def id:Parser[String] = rep(ident | "_") ^^ { case l => l.foldLeft("")(_+_) }

  def oparg:Parser[Operand] = reg | imm | loc | gp | fp | local
  def reg = "(" ~> numLit <~ ")" ^^ { case n => Register(n) }
  def imm = numLit ^^ { case n => Immediate(n) }
  def local = ident ~ "#" ~ (numLit ^^ {case n => Some(n)} | "?" ^^ {case _ => None}) ^^ { case s ~ _ ~ n => Local(s,n) }
  def loc = "[" ~> numLit <~ "]" ^^ { case n => Location(n) }
  def gp = "GP" ^^ { case _ => GlobalPointer }
  def fp = "FP" ^^ { case _ => FramePointer }

  //Main parser for SIR ops
  def op:Parser[SIR] = (
    ("br" ~> loc ^^ {case l => Br(l)}) |
      ("blbc" ~> oparg ~ loc ^^ {case o ~ l => Blbc(o,l)}) |
      ("blbs" ~> oparg ~ loc ^^ {case o ~ l => Blbs(o,l)}) |
      ("call" ~> loc ^^ {case l => Call(l)}) |
      ("ret" ~> imm ^^ {case n => Ret(n)}) |
      ("nop" ^^ {case _ => Nop}) |
      ((ident ~ rep(oparg ^^ {case o => o.toString}) ~ opt(":" ~ typ ^^{case a ~ b => a+b.toString}))
      ^^ {
        case s ~ ls ~ Some(sl) => Other(List(s)++ls++List(sl))
        case s ~ ls ~ None => Other(List(s)++ls)
      })
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

  def runParser(in:String):Either[SIR,Declaration] = {
    val tokens = new lexical.Scanner(in)
    val p = phrase(opcode)(tokens)
    p match {
      case Success(r,_) => {
        println(in)
        println(r)
        r
      }
      case _ => throw new RuntimeException("bad parse on input: " + in)
    }
  }
}
