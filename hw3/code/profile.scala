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

  def cleanUpCounters(cfgs:List[CFG]) = {
    def cleanUp(b:Block) = {
      var l:ListBuffer[SIR with Instr] = new ListBuffer()
      var dropNext = false
      for (i <- b.instrs.reverse) {
        if (!dropNext) {
          i match {
            case Count(Immediate(_)) =>
            case Count(Register(_)) => dropNext = true
            case x => l.append(x)
          } 
        } else {
            dropNext = false
        }
      }
      b.instrs = l.reverse
    }
    cfgs.foreach(_.list.foreach(cleanUp))
    def insertFallbacks(c:CFG) = {
      var prev:Block = c.list.head
      for (b <- c.list.tail) {
        prev.nextBlock = b
        prev = b
      }
      def insertFallback(b:Block) = {
        b.instrs.last match {
          case Blbs(_,_,_) => {
            val i = Br(Dest(b.nextBlock))
            i.num = util.instr_count
            util.instr_count += 1
            b.instrs.append(i)
          }
          case Blbc(_,_,_) => {
            val i = Br(Dest(b.nextBlock))
            i.num = util.instr_count
            util.instr_count += 1
            b.instrs.append(i)
          }
          case _ => 
        }
      }
      c.list.foreach(insertFallback)
    }
    cfgs.foreach(insertFallbacks)
  }

  def instrBranches(cfgs:List[CFG]):HashMap[(Block,Block),Int] = {
    var counter_counter = 0
    var branch_counters = new HashMap[(Block,Block),Int]()
    def addBranchCounter(b:Block) {
      val (body,branch,fallback) = if (b.instrsSSA.length > 1) {
        b.instrsSSA.dropRight(1).last match {
          case a:Blbc => (b.instrsSSA.dropRight(2),a,List(b.instrsSSA.last))
          case a:Blbs => (b.instrsSSA.dropRight(2),a,List(b.instrsSSA.last))
          case _ => (b.instrsSSA.init,b.instrsSSA.last,List())
        }
      } else {
        (List(),b.instrsSSA(0),List())
      }
      def countBranch(branch:SSA,curr:Block,des:Operand):List[SSA] = {
        val dest = des match {
          case Dest(block) => block
          case _ => throw new RuntimeException("blah")
        }
        counter_counter += 1
        val cinstr = Count(Immediate(counter_counter))
        util.instr_count += 1
        cinstr.num = util.instr_count
        branch_counters.put((curr,dest),counter_counter)
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
        branch_counters.put((curr,fdest),counter_counter-1)
        branch_counters.put((curr,tdest),counter_counter)
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
      b.instrsSSA = body ++ instrBranch ++ fallback
    }
    cfgs.foreach(_.list.foreach(addBranchCounter))
    branch_counters
  }
}
