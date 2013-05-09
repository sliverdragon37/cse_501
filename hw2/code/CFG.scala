import scala.collection.mutable._
import scala.collection.immutable

class Block(pStart:Int, pEnd:Int, pName:String, pBlocks:ListBuffer[Block]){
  //used for initialization
  var start:Int = pStart
  var end:Int = pEnd
  def name = pName + "_" + instrs.head.num
  var blocks:ListBuffer[Block] = pBlocks
  pBlocks.append(this)
  var preds:ListBuffer[Block] = new ListBuffer[Block]()
  var succs:ListBuffer[Block] = new ListBuffer[Block]()

  //parent in the idom tree
  var idom:Block = null
  var idom_level:Int = -1

  //children in the idom tree
  var children:ListBuffer[Block] = new ListBuffer[Block]()

  //code represented like this once CFG is made
  var instrs:ListBuffer[SIR with Instr] = null

  //code represented like this once in SSA form
  var instrsSSA:List[SSA] = null

  //list of the variables that need phi nodes at the start of this
  //block
  var phi:List[Phi] = List()

  private var renamed = false

  def toGraphVizDom:String = {
    val s = name + ";\n"
    val parent = name + " -> " + idom.name + ";\n"
    s + parent
  }

  def toGraphViz:String = {
    val s = name + ";\n"
    //val pre = succs.map(name + " -> " + _.name + ";\n").foldLeft("")(_+_)
    val out = instrs.last match {
      case Br(Dest(b)) => name + " -> " + b.name + ";\n"
      case Blbc(_,Dest(b),Some(Dest(c))) => name + " -> " + b.name + ";\n" + name + " -> " + c.name + ";\n"
      case Blbs(_,Dest(b),Some(Dest(c))) => name + " -> " + b.name + ";\n" + name + " -> " + c.name + ";\n"
      case _ => ""
    }
    s + out
  }

  def allocate(i:Int) = {
    def e(o:SIR with Instr):Boolean = {
      o match {
        case Enter(_) => true
        case _ => false
      }
    }
    val idx = instrs.indexWhere(e)
    val old = instrs(idx)
    instrs(idx) = Enter(Immediate(-1*i))
    instrs(idx).num = old.num
  }

  def fixLocals(lc:immutable.Map[String,Local]) = {
    def replace(o:Operand):Operand = {
      o match {
        case x:SSALocalVar => {
          lc(x.toString)
        }
        case _ => o
      }
    }
    instrs.foreach(_.opMap(replace))
  }

  def getLocals = {
    def g(i:SIR with Instr):List[SSALocalVar] = {
      def one(a:Operand) = {
        a match {
          case s:SSALocalVar => List(s)
          case _ => List()
        }
      }
      i match {
        case x:Op => one(x.a)
        case x:Opt => one(x.a)
        case x:Opop => one(x.a) ++ one(x.b)
        case x:Opopt => one(x.a) ++ one(x.b)
        case x:Opopop => one(x.a) ++ one(x.b) //x.c is never a local
        case _ => List()
      }
    }
    instrs.flatMap(g)
  }

  def renumber(i:Int,m:HashMap[Int,Int]):Int = {
    var x = i
    for (instr <- instrs) {
      m.put(instr.num,x)
      instr.num = x
      x += 1
    }
    x
  }

  def reRegister(m:HashMap[Int,Int]) = {
    def r(o:Operand):Operand = {
      o match {
        case Register(n) => Register(m(n))
        case Location(n) => Location(m(n))
        case _ => o
      }
    }
    instrs.foreach(_.opMap(r))
  }

  def fromSSA:List[(Block,SIR with Instr)] = {

    //move instructions to insert
    var toInsert:List[(Block,SIR with Instr)] = List()

    def conv(instr:SSA):List[SIR with Instr] = {
      instr match {
        case p:Phi => {
          p.args.foreach({ b => {
            val block = b._1
            val loc = b._2
            //don't insert dead moves
            if (b._2 != DEAD) {
              toInsert ::= ((block,Move(b._2,p.ssa)))
            }
          }})
          List()
        }
        case _ => List(instr.asInstanceOf[SIR with Instr])
      }
    }

    instrs = new ListBuffer[SIR with Instr]
    instrsSSA.flatMap(conv).filter(_.live).foreach(instrs.append(_))

    toInsert
  }

  def consolidateLocals(symTable:HashMap[String,Local]):Unit = {
    def consolidate(o:Operand):Operand = {
      o match {
        case z:Local => {
          symTable.get(z.s) match {
            case Some(l) => l
            case None => {
              symTable += (z.s -> z)
              z
            }
          }
        }
        case _ => o
      }
    }
    instrs.map(_.opMap(consolidate))
  }

  def rename:Unit = {

    if (renamed)
      return

    renamed = true

    //rename all of our phi functions
    phi.foreach(_.gen)

    //all the locals in this block
    //so we can pop their stacks later
    var locals = new HashSet[Local]()

    //turn all instrs into SSA versions
    def use(o:Operand):Operand = {
      o match {
        case l:Local => l.getCurr
        case _ => o
      }
    }
    def define(o:Operand):Operand = {
      o match {
        case l:Local => {
          locals += l
          l.genName
        }
        case _ => o
      }
    }
    def convert(instr:SIR with Instr):SSA = {
      val ssa = instr match {
        case Enter(a) => Enter(use(a))
        case Entrypc => Entrypc
        case Br(a) => Br(use(a))
        case Blbc(a,b,c) => Blbc(use(a),use(b),c)
        case Blbs(a,b,c) => Blbs(use(a),use(b),c)
        case Call(a) => Call(use(a))
        case Ret(a) => Ret(use(a))
        case Nop() => Nop()
        case Add(a,b,t) => Add(use(a),use(b),t)
        case Sub(a,b,t) => Sub(use(a),use(b),t)
        case Mul(a,b,t) => Mul(use(a),use(b),t)
        case Div(a,b,t) => Div(use(a),use(b),t)
        case Mod(a,b,t) => Mod(use(a),use(b),t)
        case Neg(a,t) => Neg(use(a),t)
        case Cmpeq(a,b,t) => Cmpeq(use(a),use(b),t)
        case Cmple(a,b,t) => Cmple(use(a),use(b),t)
        case Cmplt(a,b,t) => Cmplt(use(a),use(b),t)
        case Isnull(a,t) => Isnull(use(a),t)
        case Istype(a,b,t) => Istype(use(a),use(b),t)
        case Load(a,t) => Load(use(a),t)
        case Store(a,t) => Store(use(a),t)
        case Move(a,b) => {
          Move(use(a),define(b))
        }
        case New(a,t) => New(use(a),t)
        case Newlist(a,t) => Newlist(use(a),t)
        case Checknull(a,t) => Checknull(use(a),t)
        case Checktype(a,b,t) => Checktype(use(a),use(b),t)
        case Checkbounds(a,b) => Checkbounds(use(a),use(b))
        case Lddynamic(a,b,t) => Lddynamic(use(a),use(b),t)
        case Stdynamic(a,b,Some(c)) => Stdynamic(use(a),use(b),Some(use(c)))
        case Write(a) => Write(use(a))
        case Wrl() => Wrl()
        case Param(a) => Param(use(a))
      }
      ssa.num = instr.num
      ssa
    }
    instrsSSA = phi ++ instrs.map(convert)

    //insert phi args on our successors IN THE CFG
    succs.foreach(_.insertPhiArgs(this))

    //recursively call rename on our successors IN THE DOMINATOR TREE
    children.foreach(_.rename)

    //pop the stacks
    locals.foreach(_.pop)

  }

  def insertPhiArgs(b:Block) {
    phi.foreach({p => p.add(b,p.a.getCurr)})
  }

  def insertPhi(v:Local) {
    phi ::= (Phi(v))
  }

  def printInstrs {
    println(name)
    instrs.foreach(println)
    println()
  }

  def printSSA {
    println(name)
    instrsSSA.foreach(println)
    println()
  }

  def firstInstrLocation = {
    if (instrs != null) {
      instrs.head.num
    } else {
      start
    }
  }

  def finishConstruction(iMap:HashMap[Int,SIR with Instr],bMap:immutable.Map[Int,Block]) = {
    //grab all instructions but the final branch and add them to our list of instrs
    instrs = new ListBuffer[SIR with Instr]()
    (start to (end - 1)).foreach({i => instrs.append(iMap(i))})

    //fix up the final branch to point to a block, not an instr number
    val newEnd = iMap(end) match {
      //is an explicit branch at the end of this block
      case Br(Location(n)) => Br(Dest(bMap(n)))
      case Blbc(a,Location(n),_) => {
        val nn = Blbc(a,Dest(bMap(n)),Some(Dest(bMap(end + 1))))
        nn.num = iMap(end).num
        instrs.append(nn)
        val m = Br(Dest(bMap(end + 1)))
        m.num = util.instr_count
        util.instr_count += 1
        m
      }
      case Blbs(a,Location(n),_) => {
        val nn = Blbs(a,Dest(bMap(n)),Some(Dest(bMap(end + 1))))
        nn.num = iMap(end).num
        instrs.append(nn)
        val m = Br(Dest(bMap(end + 1)))
        m.num = util.instr_count
        util.instr_count += 1
        m
      }
      case r:Ret => r
        //there is no explicit branch, just fall through
        //add an explicit branch to next block
      case x:SIR => {
        instrs.append(x)
        val n = Br(Dest(bMap(end + 1)))
        n.num = util.instr_count
        util.instr_count += 1
        n
      }
    }
    if (newEnd.num == -1) 
      newEnd.num = iMap(end).num
    instrs.append(newEnd)
    
  }

  //convert a single basic block to SSA
  //return the set of locals defined
  def findDefs:Set[Local] = {
    var definedVars = new HashSet[Local]()
    def c(instr:SIR with Instr) = {
      instr match {
        case Move(a,b:Local) => {
          definedVars += b
        }
        case _ =>
      }
    }
    instrs.foreach(c)
    definedVars
  }

  def handleBranch(instrLoc:Int, branchTo:Int, conditional:Boolean, upstreamBr:Boolean){
    // handle splitting this block
    if (instrLoc < this.end){
      this.splitBlock(instrLoc + 1, conditional)
    }

    // handle the block branched TO
    var block:Block = findBlock(branchTo)
      if (branchTo > block.start){
        if (!upstreamBr){
          if (block != this){
            this.addEdge(block.splitBlock(branchTo, true))
          }
          else{
            var newBlock = block.splitBlock(branchTo, true)
            newBlock.addEdge(newBlock)
          }
        }
        else{
          if (block != this){
            this.addEdge(block.splitBlock(branchTo, false))
          }
          else{
            var newBlock = block.splitBlock(branchTo, false)
            newBlock.addEdge(newBlock)
          }

        }
      }
      else{
        this.addEdge(block)
      }

  }

  def splitBlock(loc:Int, conditional:Boolean) = {
    var newBlock = new Block(loc, this.end, "Block_" +
      ('A'.toInt -1 + blocks.length).toChar, blocks)
    this.end = loc -1
    this.replaceEdges(newBlock)

    if (conditional){
      this.addEdge(newBlock)
    }

    newBlock
  }

  def findBlock(branchTo:Int) = {
    var i:Int = 0

    while (i < blocks.length-1 && !(blocks(i).start <= branchTo && blocks(i).end >= branchTo)){
      i+=1
    }

    blocks(i)
  }

  // PRE: this block is defined and newBlock is defined
  // POST: All successors of this block have been switched to be
  //successors of newBlock, accounting for both directions
  def replaceEdges(newBlock:Block){
    this.succs.foreach{b => this.removeEdge(b); newBlock.addEdge(b)}
  }

  def addEdge(otherBlock:Block){
    this.succs.append(otherBlock)
    otherBlock.preds.append(this)
  }

  def removeEdge(otherBlock:Block){
    this.succs.remove(this.succs.indexOf(otherBlock))
    otherBlock.preds.remove(otherBlock.preds.indexOf(this))
  }

  override def toString:String = start.toString
}


class CFG(header:MethodDeclaration, pEnd:Int, instrMap:HashMap[Int,SIR with Instr]){
  var root:Block = new Block(header.start, pEnd-1, "Root", new ListBuffer[Block])
  var name:String = header.name
  var start:Int = header.start
  var end:Int = pEnd-1
  instrMap.values.foreach(instr => handleInstr(instr, instrMap))
  lazy val list:ListBuffer[Block] = getTopoList

  //mapping of local variables to all the blocks where they're defined
  var symTable:Map[Local,HashSet[Block]] = null

  var postSSAHeader:MethodDeclaration = null

  def toGraphViz:String = {
    "digraph G {" + list.map(_.toGraphViz).foldLeft("")(_+_) + "}"
  }

  def toGraphVizDom = {
    "digraph G {" + list.map(_.toGraphVizDom).foldLeft("")(_+_) + "}"
  }

  def dumpGraphViz(s:String) = {
    val outfname = s + name + ".gv"
    val out_file = new java.io.FileOutputStream(outfname)
    val out_stream = new java.io.PrintStream(out_file)
    out_stream.print(toGraphViz)
    out_stream.close
    val domoutname = s + name + "_dom.gv"
    val domoutfile = new java.io.FileOutputStream(domoutname)
    val domoutstream = new java.io.PrintStream(domoutfile)
    domoutstream.print(toGraphVizDom)
    domoutstream.close
  }

  def getHeader:MethodDeclaration = {
    if (postSSAHeader == null) {
      header
    } else {
      postSSAHeader
    }
  }

  def printSSA = {
    list.foreach(_.printSSA)
  }

  def toSSA = {
    //loop through dominator tree in bottom up order
    val bottom_up = list.reverse

    //calculate the dominance frontiers
    val frontier = bottom_up.map({b => b -> new HashSet[Block]()}).toMap

    bottom_up.foreach({b => {
      if (b.idom != b) {
        b.idom.children.append(b)
      }
    }})
    
    bottom_up.foreach({b => {
      b.succs.foreach({s => {
        if (!(s.idom == b)) {
          frontier(b).add(s)
        }
      }})
      b.children.foreach({c => {
        frontier(c).foreach({y => {
          if (y.idom != b) {
            frontier(b).add(y)
          }
        }})
      }})
    }})

    //go through and replace distinct local vars with single one
    //would do in parser, but only want to do per method
    var localTable = new HashMap[String,Local]()
    list.foreach(_.consolidateLocals(localTable))

    //get info for where vars are defined
    symTable = util.toMapSet(list.flatMap({b => {
      val vars = b.findDefs
      vars.map(v => v->b)}}).toList)

    //now dominance frontier is calculated
    //insert phi nodes
    symTable.keys.foreach({v => {
      var hasAlready = new HashSet[Block]()
      var everOn = new HashSet[Block]()
      var workList = new HashSet[Block]()
      symTable(v).foreach({b => {
        everOn += b
        workList += b
      }})

      while (!workList.isEmpty) {
        val x = workList.head
        workList -= x
        frontier(x).foreach({y => {
          if (!hasAlready.contains(y)) {
            y.insertPhi(v)
            hasAlready += y
            if (!everOn.contains(y)) {
              everOn += y
              workList += y
            }
          }
        }})
      }
    }})

    //now populate the phi nodes
    root.rename
    
  }

  //convert everything back to normal
  def fromSSA = {
    val toAdd = list.flatMap(_.fromSSA)
    def add(x:(Block,SIR with Instr)):Unit = {
      val b = x._1
      val instr = x._2
      //always add move instructions right before the last branch
      def isBranch(s:SIR):Boolean = {
        s match {
          case x:Blbc => true
          case x:Blbs => true
          case x:Br => true
          case x:Ret => true
          case _ => false
        }
      }
      b.instrs = b.instrs.filter(!isBranch(_)) ++ List(instr) ++ b.instrs.filter(isBranch)
    }
    toAdd.foreach(add)
    val locals = list.flatMap(_.getLocals)
    var offsets:HashMap[String,Int] = new HashMap[String,Int]()
    var curr = -4
    for (l <- locals) {
      if (!offsets.contains(l.toString)) {
        offsets += (l.toString -> curr)
        curr -= 4
      }
    }

    def genParamsAndLocals(l:List[(String,Int,IRType)],lc:immutable.Map[String,Local]):List[(String,Int,IRType)] = {
      val params = l.filter(_._2 > 0)
      val origLocals = l.filter(_._2 < 0)
      val origTypes = origLocals.map(x => x._1 -> x._3).toMap

      def getOrig(s:String):String = s.takeWhile(_ != '$')

      def makeLocal(s:String):(String,Int,IRType) = {
        val n = lc(s).n match {
          case Some(x) => x
          case None => throw new RuntimeException("no dynamic offsets for local variables")
        }
        (s,n,origTypes(getOrig(s)))
      }
      val localsUnsorted = lc.keys.map(makeLocal).toList

      val locals = localsUnsorted.sortWith({(x,y) => { x._2 < y._2}})

      params ++ locals
    }
    val localCache = offsets.keys.map({k => {k -> Local(k,offsets.get(k))}}).toMap
    postSSAHeader = MethodDeclaration(header.name,root.firstInstrLocation,genParamsAndLocals(header.paramsAndLocals,localCache))
    list.foreach(_.fixLocals(localCache))
    //make enter take the right amount of space
    root.allocate(curr)
  }

  def finishConstruction = {
    val blockMap = list.map(b => b.firstInstrLocation -> b).toMap
    list.foreach(_.finishConstruction(instrMap,blockMap))
  }

  def handleInstr(instr:SIR with Instr, instrMap:HashMap[Int,SIR with Instr]){
    instr match {
      case br: Br => addBranch(br.num, resolveOperand(br.a), false, instrMap)
      case blbc: Blbc => addBranch(blbc.num, resolveOperand(blbc.b), true, instrMap)
      case blbs: Blbs => addBranch(blbs.num, resolveOperand(blbs.b), true, instrMap)
      case n: Object => 
    }
  }

  def resolveOperand(op:Operand) = {
    op match{
      case i: Immediate => i.n
      case l: Location => l.n
      case _ => -1
    }
  }


  def addBranch(instrLoc:Int, branchTo:Int, conditional:Boolean, instrMap:HashMap[Int,SIR with Instr]){
    instrMap(branchTo-1) match {
      case br: Br => root.findBlock(instrLoc).handleBranch(instrLoc, branchTo, conditional, true)
      case o: Object => root.findBlock(instrLoc).handleBranch(instrLoc, branchTo, conditional, false)
    }

  }

  def renumber(i:Int,m:HashMap[Int,Int]):Int = {
    var x = i
    for (b <- list) {
      x = b.renumber(x,m)
    }
    x
  }

  def reRegister(m:HashMap[Int,Int]) = {
    list.foreach(_.reRegister(m))
  }

  def getTopoList() = {
    var workList:Queue[Block] = new Queue[Block]()
    var finalList:ListBuffer[Block] = new ListBuffer[Block]()
    var block:Block = null

    workList.enqueue(root)
    while (!workList.isEmpty){
      block = workList.dequeue
      finalList.append(block)

      block.succs.foreach{b => if(!finalList.contains(b) && !workList.contains(b)){workList.enqueue(b)}}
    }

    finalList
  }

  def printInstrs = {
    list.foreach(_.printInstrs)
  }

  override def toString:String = {
    var str:String = name + "\n"
    list.foreach(b => str += ("# " + b.start + "\n\t"  + "Preds: " + b.preds.toList.mkString(", ") + "\n\t" + "Succs: " + b.succs.toList.mkString(", ") + "\n\t" + "idom: " + b.idom + "\n"))
    str
  }
}

object CFGFactory{
  def makeAllCFGs(headers:List[Declaration],instrs:List[SIR with Instr]) = {

    var instrList = instrs

    // find the method headers and get start points
    val methodList = headers collect { case m: MethodDeclaration => m}
    val methodEnds = (methodList.map(elem => elem.start).drop(1) ++ List(instrList.length)).toArray

    var CFGs = new ListBuffer[CFG]
    (0 to methodList.length - 1).foreach({ 
      i => {
        var instrHashMap = new HashMap[Int, SIR with Instr]
        instrList.drop(methodList(i).start-1).dropRight(instrList.length-(methodEnds(i)-1)).foreach(elem => instrHashMap += (elem.num -> elem))
        CFGs.append(new CFG(methodList(i), methodEnds(i), instrHashMap))
      }
    })
    CFGs.toList
  }
}

