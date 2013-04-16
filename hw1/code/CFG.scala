import scala.collection.mutable._

class Block(pStart:Int, pEnd:Int, pName:String, pBlocks:ListBuffer[Block]){
  var start:Int = pStart
  var end:Int = pEnd
  val name:String = pName
  var blocks:ListBuffer[Block] = pBlocks
  pBlocks.append(this)
  var preds:ListBuffer[Block] = new ListBuffer[Block]()
  var succs:ListBuffer[Block] = new ListBuffer[Block]()
  var idom:Block = null
  var idom_level:Int = -1

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


class CFG(header:MethodDeclaration, pEnd:Int, instrMap:HashMap[Int,SIR]){
  var root:Block = new Block(header.start, pEnd-1, "Root", new ListBuffer[Block])
  var name:String = header.name
  var start:Int = header.start
  var end:Int = pEnd-1
  instrMap.values.foreach(instr => handleInstr(instr, instrMap))
  var list:ListBuffer[Block] = getTopoList

  def handleInstr(instr:SIR, instrMap:HashMap[Int,SIR]){
    instr match {
      case br: Br => addBranch(br.num, resolveOperand(br.dest), false, instrMap)
      case blbc: Blbc => addBranch(blbc.num, resolveOperand(blbc.dest), true, instrMap)
      case blbs: Blbs => addBranch(blbs.num, resolveOperand(blbs.dest), true, instrMap)
      case n: Object => 
    }
  }

  def resolveOperand(op:Operand) = {
    op match{
     case r: Register => -1
     case i: Immediate => i.n
     case l: Local => -1
     case l: Location => l.n
    }
  }


  def addBranch(instrLoc:Int, branchTo:Int, conditional:Boolean, instrMap:HashMap[Int,SIR]){
    instrMap(branchTo-1) match {
      case br: Br => root.findBlock(instrLoc).handleBranch(instrLoc, branchTo, conditional, true)
      case o: Object => root.findBlock(instrLoc).handleBranch(instrLoc, branchTo, conditional, false)
    }

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

  override def toString:String = {
    if (list == null){
      list = getTopoList()
    }
    var str:String = name + "\n"
    list.foreach(b => str += ("# " + b.start + "\n\t"  + "Preds: " + b.preds.toList.mkString(", ") + "\n\t" + "Succs: " + b.succs.toList.mkString(", ") + "\n\t" + "idom: " + b.idom + "\n"))
    str
  }
}

object CFGFactory{
  def makeAllCFGs(sirList:List[Either[SIR, Declaration]]) = {

    // find the method headers and get start points
    var headerList =  sirList.filter(elem => elem.isRight).map(elem => elem.right.get)
    var instrList = sirList.filter(elem => elem.isLeft).map(elem => elem.left.get)


    var methodList = headerList collect { case m: MethodDeclaration => m}
    var methodEnds = methodList.map(elem => elem.start).drop(1).to[ListBuffer]
    methodEnds.append(instrList.length)

    var CFGs = new ListBuffer[CFG]
    var instrHashMap = new HashMap[Int, SIR]
    var i = 0
    for(i <- 0 to methodList.length -1){
      instrHashMap.clear
      instrList.drop(methodList(i).start-1).dropRight(instrList.length-(methodEnds(i)-1)).foreach(elem => instrHashMap += (elem.num -> elem))
      CFGs.append(new CFG(methodList(i), methodEnds(i), instrHashMap))
    }
    CFGs.toList
  }
}

