import scala.collection.mutable._

object profile2 {
  def fixRegisters(cfgs:List[CFG]){

    def fixBlockRegisters(b:Block){
      def fixSequenceRegister(l:Load, a:Add, c:Count){
        a.a = Register(l.num)
        c.a = Register(a.num)
      }

      if (b.instrsSSA.size > 3){
        var load = b.instrsSSA.head
        var add = b.instrsSSA.tail.head
        var count = b.instrsSSA.tail.tail.head
        var remaining = b.instrsSSA.tail.tail.tail
        while (remaining.size > 0){
          count match {
            case c:Count => add match{ 
              case a:Add => load match{ 
                case l:Load => fixSequenceRegister(l,a,c);
                case _ =>
              }
              case _ =>
            }
            case _ => 
          }
          load = add; add = count; count = remaining.head; remaining =
        remaining.tail;
        }
      }
    }

    cfgs.foreach(_.list.foreach(fixBlockRegisters))
  }

  def countDynamic(cfgs:List[CFG]) {
    var counter_counter = 0
    val    dynamicCounters = new HashMap[Int,(Block, SSA)]

    def addDynamicCounter(b:Block) {

      // Insert load, add, count on the target to determine type
      // Assumes loadTarget will always be a register due to the Start
      //front-end parsing
      def insertCounterWithRegister(loadTarget:Register,
      currentInstr:Instr): List[SSA] = {
        val load = Load(loadTarget, IntType)
        val shifted_counter = counter_counter << 16
        val add = Add(Register(currentInstr.num), Immediate(shifted_counter),
      IntType)
        val count = Count(Register(currentInstr.num))

        counter_counter += 1
        val result = List(load, add, count)
        result
      }

      def insertCounter(loadTarget:Operand, currentInstr:Instr): List[SSA] = {
        loadTarget match {
          case r:Register => insertCounterWithRegister(r, currentInstr)
          case _ => List()
        }
      }


      var start:List[SSA] = List()
      var end = b.instrsSSA.last

      var current = b.instrsSSA.head
      var remaining = b.instrsSSA.tail
      while (current != end){
        // if current is a dynamic load, insert the type counter
        //instructions in front of it
        current match {
          case ld:Lddynamic => start = start ++ insertCounter(ld.a, ld)
          case sd:Stdynamic => start = start ++ insertCounter(sd.a, sd)
          case _ =>
        }

        // shift this whole complex along the block...
        start = start :+ current
        current = remaining.head
        remaining = remaining.tail
      }

      start = start :+ current
      b.instrsSSA = start

    }

    cfgs.foreach(_.list.foreach(addDynamicCounter))
  }

}
