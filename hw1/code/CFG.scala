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

  def handleBranch(instrLoc:Int, branchTo:Int){
    // handle splitting this block
    if (instrLoc < this.end){
      this.splitBlock(instrLoc + 1)
    }

    // handle the block branched TO
    var block:Block = findBlock(branchTo)
    if (branchTo > block.start){
      this.addEdge(block.splitBlock(branchTo))
    }
    else{
      this.addEdge(block)
    }

  }

  def splitBlock(loc:Int) = {
    var newBlock = new Block(loc, this.end, "Block_" +
      ('A'.toInt -1 + blocks.length).toChar, blocks)
    this.end = loc -1
    this.replaceEdges(newBlock)
    this.addEdge(newBlock)
    newBlock
  }

  def findBlock(branchTo:Int) = {
    var i:Int = 0

    while (i < blocks.length && !(blocks(i).start <= branchTo && blocks(i).end >= branchTo)){
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

  override def toString:String = "(" + name + ", " + start + ", " + end + ")"
}


class CFG(){
  var root:Block = new Block(1, 10, "Root", new ListBuffer[Block])
  var list:ListBuffer[Block] = null

  def addBranch(instrLoc:Int, branchTo:Int){
    root.findBlock(instrLoc).handleBranch(instrLoc, branchTo)
    println(root.blocks)
  }

  def getTopoList() = {

    if (list == null) {

      var workList:Queue[Block] = new Queue[Block]()
      var finalList:ListBuffer[Block] = new ListBuffer[Block]()
      var block:Block = null

      workList.enqueue(root)
      while (!workList.isEmpty){
        block = workList.dequeue
        finalList.append(block)

        block.succs.foreach{b => if(!finalList.contains(b) && !workList.contains(b)){workList.enqueue(b)}}
      }

      list = finalList
    }
    list
  }

  override def toString:String = {
    var list:ListBuffer[Block] = getTopoList()
    var str:String = "[" + list.length + ",\n"
    list.foreach(b => str += b + ": { " + b.succs+ "}\n")
    str += "]"
    str
  }
}

object Test{
  def main(args:Array[String]){
    var cfg:CFG = new CFG()
    println(cfg)
    cfg.addBranch(5, 1)
    println(cfg)
    cfg.addBranch(8, 3)
    println(cfg)
  }
}
