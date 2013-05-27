
//take in program output, make a program that generates the same output
object memoize {
  def makeEquivProg(s:Stream[String]):List[String] = {
    var l:List[String] = List()
    def conv(s:String):List[SIR with Instr] = {
      val x = s.split(" ").filter(_.length > 0).map((y:String) => Write(Immediate(Integer.parseInt(y))))
      (x ++ List(Wrl())).toList
    }
    val header = List(MethodDeclaration("main",2,List()))
    val body = List(Entrypc) ++ s.flatMap(conv) ++ List(Ret(Immediate(0)))
    var i = 1
    for (stm <- body) {
      stm.num = i
      i += 1
    }
    (header ++ body).map(_.toString).map(_ + "\n")
  }
}
