import scala.collection.mutable._

object dom {

  def intersect(a:Block,b:Block):Block = {

    var f1 = a
    var f2 = b
    if (a == b) {
      return a
    }
    if (b == null) {
      return a
    }
    if (a == null) {
      return b
    }
    while (f1 != f2) {
      if (f1.idom_level >= f2.idom_level) {
        f1 = f1.idom
      }
      if (f2.idom_level >= f1.idom_level && f1 != f2) {
        f2 = f2.idom
      }
    }
    f1
  }

  def find_dominator(c:CFG) {
    //root of cfg
    var start_node = c.root
    //all non-root blocks in topo order
    val blocks = c.getTopoList.drop(1)

    start_node.idom = start_node

    var changed = true

    // var worklist:Queue[Block] = new Queue[Block]()
    // var hs:HashSet[Block] = new HashSet[Block]()
    // worklist.enqueue(start_node)
    // hs.add(start_node)
    // blocks.foreach(worklist.enqueue(_))
    // blocks.foreach(hs.add(_))

    // while (!worklist.isEmpty) {
    //   val b = worklist.dequeue
    //   hs.remove(b)
    //   var new_idom:Block = null
    //   for (pred <- b.preds) {
    //     if (pred.idom != null) {
    //       new_idom = intersect(pred,new_idom)
    //     }
    //   }
    //   if (b.idom != new_idom && new_idom != null) {
    //     b.idom = new_idom
    //     b.idom_level = new_idom.idom_level + 1
    //     b.succs.foreach({block => {
    //       if (!hs.contains(block)) {
    //         hs.add(block)
    //         worklist.enqueue(block)
    //       }
    //     }})
    //   }
    // }

    while (changed) {
      changed = false
      for (b <- blocks) {
        var new_idom = b.preds.filter(x => x.idom != null).head
        for (pred <- b.preds) {
          if (pred.idom != null) {
            new_idom = intersect(pred,new_idom)
          }
        }
        if (b.idom != new_idom) {
          b.idom = new_idom
          changed = true
        }
      }
    }
  }

  def run_test() {

    var l = new ListBuffer[Block]()

    var a = new Block(1,1,"a",l)
    var b = new Block(1,1,"b",l)
    var c = new Block(1,1,"c",l)
    var d = new Block(1,1,"d",l)
    var e = new Block(1,1,"e",l)
    var f = new Block(1,1,"f",l)
    var g = new Block(1,1,"g",l)
    var h = new Block(1,1,"h",l)

    a.succs = new ListBuffer[Block]() ++ List(b)
    b.succs = new ListBuffer[Block]() ++ List(c,d)
    c.succs = new ListBuffer[Block]() ++ List(e)
    d.succs = new ListBuffer[Block]() ++ List(e)
    e.succs = new ListBuffer[Block]() ++ List(f,g)
    f.succs = new ListBuffer[Block]() ++ List(b,h)
    g.succs = new ListBuffer[Block]() ++ List(b)
    
    b.preds = new ListBuffer[Block]() ++ List(a,f,g)
    c.preds = new ListBuffer[Block]() ++ List(b)
    d.preds = new ListBuffer[Block]() ++ List(b)
    e.preds = new ListBuffer[Block]() ++ List(c,d)
    f.preds = new ListBuffer[Block]() ++ List(e)
    g.preds = new ListBuffer[Block]() ++ List(e)
    h.preds = new ListBuffer[Block]() ++ List(f)

    var test = new CFG()

    test.root = a

    find_dominator(test)

    for (block <- List(a,b,c,d,e,f,g,h)) {
      println(block.name + " has idom: " + block.idom.name)
    }
  }

}
