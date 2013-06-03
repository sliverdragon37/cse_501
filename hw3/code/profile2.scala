import scala.collection.mutable._

object profile2 {
  def optimizeDynamic(cfgs:List[CFG], counterMap:HashMap[Int, (Block,
    SIR with Instr, String)], resultsMap:HashMap[Int,Array[Int]], types:List[TypeDeclaration]){

    def optimizeLddynamic(b:Block, cinstr:SIR with Instr, newOffset:Int){
      System.out.println("RUNNING FOR OFFSET: " + newOffset)
      // find the cinstr in the block
      if (b.instrs.size > 8){
        // spanning from getting the location in memory for the object
        //base pointer, through the inserted counts, through the
        //dynamic load and checks, and up to the add just before the
        //load
        val window = b.instrs.sliding(8)
        val countLoc = 3
        val offsetAddLoc = 7

        var windowIndex = 0
        var curWindow = window.next

        // iterate until the window is positioned properly
        while (window.hasNext && curWindow(countLoc) != cinstr){
          windowIndex += 1
          curWindow = window.next
        }

        // Do this all nested to be on the safe side in case any part
        // of the IR layout isn't exactly as expected
        curWindow(0) match {
          case cn1:Checknull => curWindow(1) match {
            case l:Load => curWindow(2) match {
              case a1:Add => curWindow(3) match {
                case c:Count => curWindow(4) match{
                  case ld:Lddynamic => curWindow(5) match{
                    case cn2:Checknull => curWindow(6) match{
                      case ct:Checktype => curWindow(7) match{
                        case a2:Add => 
                          a2.a = Register(cn1.num)
                          a2.b match {
                            case o:Offset => a2.b = Offset(o.s, Option(newOffset))
                            case _ =>
                          }
                        case _ =>
                      } 
                      case _ =>
                    }
                    case _ =>
                  }
                  case _ =>
                }
                case _ =>
              }
            case _ =>
            }
            case _ =>
          }
          case _ =>
        }


        val leftHalf =b.instrs.toList.take(windowIndex + 1) :+
        curWindow.last 
        val temp = new ListBuffer[SIR with SIR with Instr]()
        b.instrs = temp ++ (leftHalf ++
          b.instrs.takeRight(b.instrs.size - (curWindow.size +
        windowIndex)))

      }
    }

    def offsetLookup(index:Int, name:String):Int = {
      var offset = 0
      types.foreach(w => w.args.foreach(y => System.out.println(y._1 +
      " " + y._2)))
      if (index >= 5){
        offset = (types(index - 5).args.find(x => x._1.equals(name))) match{
          case Some(tuple) => tuple._2
          case None => 0
        }
      }
      System.out.println("INDEX: " + index + " : " + name + " : " + offset)
      offset
    }

    val singleTypeResults = resultsMap.filter(w => w._2.sum == w._2.max
      && w._2.sum > 0)


    singleTypeResults.foreach(w => {System.out.println("ARRAY: " + w._1); (w._2.foreach(x =>System.out.println(x)))})
    singleTypeResults.foreach(w =>
      optimizeLddynamic(counterMap(w._1)._1, counterMap(w._1)._2,
      offsetLookup(w._2.indexWhere(_ > 0), counterMap(w._1)._3)))
    
    }


  def getDynamicCounts(prof:Stream[String], numberOfTypes:Int):HashMap[Int,Array[Int]] = {
    val type_counts = new HashMap[Int,Array[Int]]()
    //grab counter values
    var c = false
    var groupNumber = -1
    var typeCounts:Array[Int] = new Array[Int](numberOfTypes)

    for (line <- prof) {
      if (c){
        val l = line.split(":")
        val counter = Integer.parseInt(l(0).trim)
        val value = Integer.parseInt(l(1).trim)

        val counterNumber = counter >> 16
        val typeNumber = (counter << 16) >> 16


        if (groupNumber != counterNumber) {
          // put the old array into the map
          type_counts.put(groupNumber,typeCounts)

          // start on a new group of counts with a new array
          groupNumber = counterNumber
          typeCounts  = new Array[Int](numberOfTypes)

        }
          typeCounts(typeNumber) = value

      }
      if (line.startsWith("- Counts :")) {
        c = true
      }
    }
    type_counts.put(groupNumber,typeCounts)
    type_counts
  }

  def fixRegisters(cfgs:List[CFG]){

    def fixBlockRegisters(b:Block){
      def fixSequenceRegister(l:Load, a:Add, c:Count){
        a.a = Register(l.num)
        c.a = Register(a.num)
      }

      if (b.instrs.size > 3){
        val window = b.instrs.sliding(3)

        while (window.hasNext){
          val curWindow = window.next
          curWindow(2) match {
            case c:Count => curWindow(1) match{ 
              case a:Add => curWindow(0) match{ 
                case l:Load => fixSequenceRegister(l,a,c);
                case _ =>
              }
              case _ =>
            }
            case _ => 
          }
        }
      }
    }

    cfgs.foreach(_.list.foreach(fixBlockRegisters))
  }

  def countDynamic(cfgs:List[CFG]):HashMap[Int,(Block, SIR with Instr, String)] = {
    var counter_counter = 0
    val    dynamicCounters = new HashMap[Int,(Block, SIR with Instr, String)]

    def addDynamicCounter(b:Block) {

      // Insert load, add, count on the target to determine type
      // Assumes loadTarget will always be a register due to the Start
      //front-end parsing
      def insertCounterWithRegister(loadTarget:Register,
      currentInstr:SIR with Instr, fieldName:String): List[SIR with Instr] = {
        val load = Load(loadTarget, IntType)
        val shifted_counter = counter_counter << 16
        val add = Add(Register(currentInstr.num), Immediate(shifted_counter),
      IntType)
        val count = Count(Register(currentInstr.num))

        dynamicCounters.put(counter_counter, (b, count, fieldName.split("_offset")(0)))

        counter_counter += 1
        val result = List(load, add, count)
        result
      }

      def insertCounter(target:Operand, offset:Operand, currentInstr:SIR with Instr): List[SIR with Instr] = {
        target match {
          case r:Register => offset match {
            case o:Offset => 
              insertCounterWithRegister(r,currentInstr,
        o.s);
            case _ => List()
          }
          case _ => List()
        }
      }


      var start:List[SIR with Instr] = List()
      var end = b.instrs.last

      var current = b.instrs.head
      var remaining = b.instrs.tail
      while (current != end){
        // if current is a dynamic load, insert the type counter
        //instructions in front of it
        current match {
          case ld:Lddynamic => 
            start = start ++ insertCounter(ld.a, ld.b, ld)
          case sd:Stdynamic => 
            start = start ++ insertCounter(sd.a, sd.b, sd)
          case _ =>
        }

        // shift this whole complex along the block...
        start = start :+ current
        current = remaining.head
        remaining = remaining.tail
      }

      start = start :+ current
      b.instrs = new ListBuffer[SIR with Instr]() ++ start

    }

    cfgs.foreach(_.list.foreach(addDynamicCounter))
    dynamicCounters
  }

}
