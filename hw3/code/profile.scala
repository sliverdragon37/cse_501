import scala.collection.mutable._

object profile {

  def getBranchCounts(prof:Stream[String]):HashMap[Int,Int] = {
    val branch_counts = new HashMap[Int,Int]()
    //grab counter values
    var c = false
    for (line <- prof) {
      if (c) {
        val l = line.split(":")
        val counter = Integer.parseInt(l(0).trim)
        val value = Integer.parseInt(l(1).trim)
        branch_counts.put(counter,value)
      }
      if (line.startsWith("- Counts :")) {
        c = true
      }
    }
    branch_counts
  }
  def instrBranches(cfgs:List[CFG]):HashMap[Int,(Block,Block)] = {
    var counter_counter = 0
    var branch_counters = new HashMap[Int,(Block,Block)]()
    def addBranchCounter(b:Block) {
      val body = b.instrsSSA.init
      val branch = b.instrsSSA.last 
      def countBranch(branch:SSA,curr:Block,des:Operand):List[SSA] = {
        val dest = des match {
          case Dest(block) => block
          case _ => throw new RuntimeException("blah")
        }
        counter_counter += 1
        val cinstr = Count(Immediate(counter_counter))
        util.instr_count += 1
        cinstr.num = util.instr_count
        branch_counters.put(counter_counter,(curr,dest))
        List(cinstr,branch)
      }
      def countCondBranch(branch:SSA,a:Operand,curr:Block,tdes:Option[Operand],fdes:Option[Operand]):List[SSA] = {
        val tdest = tdes match {
          case Some(Dest(block)) => block
          case _ => throw new RuntimeException("blah")
        }
        val fdest = fdes match {
          case Some(Dest(block)) => block
          case _ => throw new RuntimeException("blah")
        }
        counter_counter += 1
        val ainstr = Add(Immediate(counter_counter),a,IntType)
        counter_counter += 1
        util.instr_count += 1
        ainstr.num = util.instr_count
        val cinstr = Count(Register(ainstr.num))
        util.instr_count += 1
        cinstr.num = util.instr_count
        branch_counters.put(counter_counter-1,(curr,fdest))
        branch_counters.put(counter_counter,(curr,tdest))
        List(ainstr,cinstr,branch)
      }
      val instrBranch = branch match {
        case Br(a) => countBranch(branch,b,a)
        case Blbc(a,x,y) => {
          countCondBranch(branch,a,b,Some(x),y)
        }
        case Blbs(a,x,y) => {
          countCondBranch(branch,a,b,y,Some(x))
        }
        case Ret(a) => List(branch)
        case _ => throw new RuntimeException("last instruction in block not branch")
      }
      b.instrsSSA = body ++ instrBranch
    }
    cfgs.foreach(_.list.foreach(addBranchCounter))
    branch_counters
  }
}
