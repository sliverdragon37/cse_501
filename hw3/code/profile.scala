
object profile {
  def instrBranches(cfgs:List[CFG]) {
    var counter_counter = 0
    def addBranchCounter(b:Block) {
      val body = b.instrsSSA.init
      val branch = b.instrsSSA.last 
      //TODO: record which numbers mean which branch somehow/somewhere
      def countBranch(branch:SSA):List[SSA] = {
        counter_counter += 1
        val cinstr = Count(Immediate(counter_counter))
        util.instr_count += 1
        cinstr.num = util.instr_count
        List(cinstr,branch)
      }
      def countCondBranch(branch:SSA,a:Operand):List[SSA] = {
        counter_counter += 1
        val ainstr = Add(Immediate(counter_counter),a,IntType)
        counter_counter += 1
        util.instr_count += 1
        ainstr.num = util.instr_count
        val cinstr = Count(Register(ainstr.num))
        util.instr_count += 1
        cinstr.num = util.instr_count
        List(ainstr,cinstr,branch)
      }
      val instrBranch = branch match {
        case Br(a) => countBranch(branch)
        case Blbc(a,b,c) => countCondBranch(branch,a)
        case Blbs(a,b,c) => countCondBranch(branch,a)
        case Ret(a) => List(branch)
        case _ => throw new RuntimeException("last instruction in block not branch")
      }
      b.instrsSSA = body ++ instrBranch
    }
    cfgs.foreach(_.list.foreach(addBranchCounter))
  }
}
