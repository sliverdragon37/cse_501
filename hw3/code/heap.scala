import scala.collection.mutable.PriorityQueue

object BlockOrd extends Ordering[Block] {
  def compare(a:Block,b:Block) = a.heapKey-b.heapKey
}

object heap {

  var pq:PriorityQueue[Block] = new PriorityQueue()(BlockOrd)
  def makeHeap() = pq = new PriorityQueue()(BlockOrd)
  def insert(b:Block,k:Int) = {
    b.heapKey = k
    b.heapInserted = true
    pq.enqueue(b)
  }
  def extractMax():Block = {
    pq.dequeue()
  }
  def isEmpty:Boolean = {
    pq.isEmpty
  }

}
