import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object SIRParser extends StandardTokenParsers {

  type Arg = (String,Int,IRType)

  def numLit:Parser[Int] = numericLit ^^ {case n => n.toInt}

  def opcode:Parser[Either[SIR,Declaration]] = 
    ( header ^^ { case h => Right(h) }) |
    (instruction ^^ { case i => Left(i) })
  def header:Parser[Declaration] = typHeader | methodHeader | globalHeader
  def typHeader:Parser[Declaration] = ("type" ~> stringLit ~ ":" ~ arglist) ^^ {case cname ~ _ ~ args => TypeDeclaration(cname,args) }
  def methodHeader:Parser[Declaration] = ("method" ~> stringLit ~ "@" ~ numLit ~ ":" ~ arglist) ^^ { case mname ~ _ ~ codeAddr ~ _ ~ args => MethodDeclaration(mname,codeAddr,args) }
  def globalHeader:Parser[Declaration] = ("global" ~> arg) ^^ {case a => GlobalDeclaration(a)}

  def arglist = rep(arg)
  def arg:Parser[Arg] = stringLit ~ "#" ~ numLit ~ ":" ~ typ ^^ {case s ~ _ ~ n ~ _ ~ t => (s,n,t)}

  def instruction:Parser[SIR] = "instr" ~> numLit ~ ":" ~ op ^^ { case n ~ _ ~ op => {
    op.num = n
    op
  }}

  def loc:Parser[Operand] = "[" ~> numLit <~ "]" ^^ { case n => Location(n) }

  def oparg:Parser[Operand] = reg | imm | local
  def reg = "(" ~> numLit <~ ")" ^^ { case n => Register(n) }
  def imm = numLit ^^ { case n => Immediate(n) }
  def local = stringLit ~ "#" ~ numLit ^^ { case s ~ _ ~ n => Local(s,n) }

  //Main parser for SIR ops
  def op:Parser[SIR] = (
    ("br" ~> loc ^^ {case l => Br(l)}) |
    ("blbc" ~> oparg ~ loc ^^ {case o ~ l => Blbc(o,l)}) |
    ("blbs" ~> oparg ~ loc ^^ {case o ~ l => Blbs(o,l)}) |
    ("call" ~> loc ^^ {case l => Call(l)}) |
    ("ret" ~> imm ^^ {case n => Ret(n)}) |
    ("nop" ^^ {case _ => Nop}) |
    (rep(stringLit) ^^ {case s => Other(s)})
  )

  def typ:Parser[IRType] = baseType ~ ptr ^^ {case b ~ n =>
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
    (stringLit ^^ {case s => UserType(s)})
  )
  def ptr:Parser[Int] = rep("*") ^^ {case l => l.length}


  def runParser(in:String):Either[SIR,Declaration] = {
    val p = phrase(opcode)
    p(new lexical.Scanner(in)) match {
      case Success(r,_) => r
      case _ => throw new RuntimeException("bad parse")
    }
  }
}
