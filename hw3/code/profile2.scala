import scala.collection.mutable._

object profile2 {
  def optimizeDynamic(cfgs:List[CFG], counterMap:HashMap[Int, (Block,
    SSA)], resultsMap:HashMap[Int,Array[Int]]){

    def optimizeLddynamic(b:Block, cinstr:SSA, newOffset:Int){
      // find the cinstr in the block
      if (b.instrsSSA.size > 8){
        // spanning from getting the location in memory for the object
        //base pointer, through the inserted counts, through the
        //dynamic load and checks, and up to the add just before the
        //load
        val window = b.instrsSSA.sliding(8)
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


        val leftHalf =b.instrsSSA.take(windowIndex + 1) :+ curWindow.last 
        b.instrsSSA = leftHalf ++
          b.instrsSSA.takeRight(b.instrsSSA.size - (curWindow.size + windowIndex))

      }
    }

    optimizeLddynamic(counterMap(0)._1, counterMap(0)._2, 8)

  }


  def getDynamicCounts(prof:Stream[String], numberOfTypes:Int):HashMap[Int,Array[Int]] = {
    val type_counts = new HashMap[Int,Array[Int]]()
    //grab counter values
    var c = false
    var groupNumber = -1
    var typeCounts:Array[Int] = new Array[Int](numberOfTypes)

    for (line <- prof) {
      if (c){
        System.out.println("C is true!")
        val l = line.split(":")
        val counter = Integer.parseInt(l(0).trim)
        val value = Integer.parseInt(l(1).trim)

        val counterNumber = counter >> 16
        val typeNumber = (counter << 16) >> 16

        System.out.println("NUBMERS: " + counterNumber + " : " +
        typeNumber + " : " + value)

        if (groupNumber != counterNumber) {
          System.out.println("NEW GROUP: " + groupNumber + " : " + counterNumber)
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

      if (b.instrsSSA.size > 3){
        val window = b.instrsSSA.sliding(3)

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

  def countDynamic(cfgs:List[CFG]):HashMap[Int,(Block, SSA)] = {
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

        dynamicCounters.put(counter_counter, (b, count))

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
    dynamicCounters
  }

}
