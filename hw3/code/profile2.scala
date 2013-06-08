import scala.collection.mutable._

object profile2 {
  def optimizeDynamic(cfgs:List[CFG], counterMap:HashMap[Int, (Block,
    SIR with Instr, String, Int)], resultsMap:HashMap[Int,Array[Int]],
  types:List[TypeDeclaration]):(Int, Int) = {

    var loadCount:Int = 0
    var storeCount:Int = 0

    def optimizeLddynamic(b:Block, cinstr:SIR with Instr,
    newOffset:(String,Int,IRType)){

      // find the cinstr in the block
      if (b.instrs.size > 4){
        val window = b.instrs.sliding(5)
        val countLoc = 3

        var windowIndex = 0
        var curWindow = window.next

        // iterate until the window is positioned properly
        while (window.hasNext && !(curWindow(countLoc) eq cinstr)){
          windowIndex += 1
          curWindow = window.next
        }

        curWindow(0) match {
          case cn:Checknull => curWindow(1) match{
            case l:Load => curWindow(2) match {
              case a:Add => curWindow(3) match{
                case c:Count => curWindow(4) match {
                  case ld:Lddynamic => {

                    a.a = Register(cn.num)
                    a.b  = Offset(newOffset._1 + "_offset", Option(newOffset._2))
                    curWindow(4) = Load(Register(a.num), DynamicType)
                    curWindow(4).num = ld.num

                    newOffset._3 match {
                      case i:PrimitiveType => {

                        val remainingInstrs =
                          b.instrs.takeRight(b.instrs.size -  (windowIndex
                            + curWindow.size))

                        if (remainingInstrs.size >= 4){


                          val newWindow = remainingInstrs.take(4)

                          val num = newWindow(3).num
                          newWindow(3) = Load(Register(a.num), newOffset._3)
                          newWindow(3).num = num


                        val leftHalf = b.instrs.toList.take(windowIndex) :+
                        curWindow(0) :+ curWindow(2)
                        b.instrs = new ListBuffer[SIR with Instr]() ++
                        ((leftHalf :+ newWindow(3)) ++
                          b.instrs.takeRight(b.instrs.size - (curWindow.size +
                            windowIndex + newWindow.size)))

                        loadCount += 1
                        }
                        else{
                          val leftHalf = b.instrs.toList.take(windowIndex) :+
                          curWindow(0) :+ curWindow(2) :+ curWindow(4)
                          b.instrs = new ListBuffer[SIR with Instr]() ++ (leftHalf ++
                            b.instrs.takeRight(b.instrs.size - (curWindow.size +
                              windowIndex)))

                          loadCount += 1
                        }

                      }
                      case _ =>{

                    

                        val leftHalf = b.instrs.toList.take(windowIndex) :+
                        curWindow(0) :+ curWindow(2) :+ curWindow(4)
                        b.instrs = new ListBuffer[SIR with Instr]() ++ (leftHalf ++
                          b.instrs.takeRight(b.instrs.size - (curWindow.size +
                            windowIndex)))

                        loadCount += 1

                      }
                    }


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
      }

}

    def optimizeStdynamic(b:Block, cinstr:SIR with Instr,
    newOffset:(String,Int,IRType)){

      // find the cinstr in the block
      if (b.instrs.size > 8){
        val window = b.instrs.sliding(7)
        val countLoc = 5

        var windowIndex = 0
        var curWindow = window.next

        // iterate until the window is positioned properly
        while (window.hasNext && !(curWindow(countLoc) eq cinstr)){
          windowIndex += 1
          curWindow = window.next
        }

        
        // Do this all nested to be on the safe side in case any part
        // of the IR layout isn't exactly as expected
        curWindow(6) match {
          case sd:Stdynamic => curWindow(5) match {
            case ct:Count => curWindow(4) match{
              case a2:Add => curWindow(3) match{
                case l:Load => curWindow(2) match{
                  case s:Store => curWindow(1) match {
                    case a1:Add => curWindow(0) match {
                      case n:New => {
                        


                            a2.a = sd.b
                            a2.b = Offset(newOffset._1 + "_offset", Option(newOffset._2))

                            curWindow(6) = Store(sd.a, Register(a2.num))
                            curWindow(6).num = sd.num
                            val leftHalf = b.instrs.toList.take(windowIndex) :+ curWindow(0) :+ curWindow(1) :+ curWindow(2) :+ curWindow(4) :+ curWindow(6)
                            b.instrs = new ListBuffer[SIR with Instr]() ++ (leftHalf ++
                              b.instrs.takeRight(b.instrs.size - (curWindow.size +
                                windowIndex)))

                            storeCount += 1
                      }
                      case _ =>
                    }
                    case _ =>
                  }
                  case cn:Checknull => {

                    a2.a = Register(cn.num)
                    a2.b = Offset(newOffset._1,
                        Option(newOffset._2))

                    curWindow(6) = Store(sd.a, Register(a2.num))
                    curWindow(6).num = sd.num

                    val leftHalf =b.instrs.toList.take(windowIndex) :+ curWindow(0) :+ curWindow(1) :+ curWindow(2) :+ curWindow(4) :+ curWindow(6)
                    b.instrs = new ListBuffer[SIR with Instr]() ++ (leftHalf ++
                      b.instrs.takeRight(b.instrs.size - (curWindow.size +
                        windowIndex)))
                    storeCount += 1
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
      }
    
    }

    def removeCounts(b:Block, cinstr:SIR with Instr){
      // find the cinstr in the block
      if (b.instrs.size > 3){
        val window = b.instrs.sliding(3)
        val countLoc = 2

        var windowIndex = 0
        var curWindow = window.next

        // iterate until the window is positioned properly
        while (window.hasNext && !(curWindow(countLoc) eq cinstr)){
          windowIndex += 1
          curWindow = window.next
        }

        val leftHalf = b.instrs.toList.take(windowIndex)
        b.instrs = new ListBuffer[SIR with Instr]() ++ (leftHalf ++
      b.instrs.takeRight(b.instrs.size - (curWindow.size + windowIndex)))
      }

    }

    def offsetLookup(index:Int, name:String):(String, Int, IRType) = {
      val offset = (types(index - 5).args.find(x => x._1.equals(name))) match{
        case Some(tuple) => tuple
        case None => null
      }
      offset
    }

    val (singleTypeResults, multiTypeResults) = resultsMap.partition(w => w._2.sum == w._2.max
      && w._2.sum > 0)
    val stores = singleTypeResults.filter(w => counterMap(w._1)._4 ==
      1)
    val loads = singleTypeResults.filter(w => counterMap(w._1)._4 ==
      0)

    // optimize loads
    loads.foreach(w =>
      optimizeLddynamic(counterMap(w._1)._1, counterMap(w._1)._2,
      offsetLookup(w._2.indexWhere(_ > 0), counterMap(w._1)._3)))

    // optimize stores
    stores.foreach(w =>
      optimizeStdynamic(counterMap(w._1)._1, counterMap(w._1)._2,
      offsetLookup(w._2.indexWhere(_ > 0), counterMap(w._1)._3)))

    // remove any counts not associated with an optimization
    multiTypeResults.foreach(w => removeCounts(counterMap(w._1)._1,
      counterMap(w._1)._2))

    def deleteCount(b:Block){
      if (b.instrs.size > 3){
        var newInstrs = new ListBuffer[SIR with Instr]()

        val iter = b.instrs.iterator
        while (iter.hasNext){
          val cur = iter.next
          cur match {
            case _:Count => {
              newInstrs.trimEnd(2)
            }
            case _ => newInstrs.append(cur)
          }
        }
        b.instrs = newInstrs
      }
    }

    cfgs.foreach(_.list.foreach(b => deleteCount(b)))

    (loadCount, storeCount)
    }

  def parseOutput(output:Stream[String]):List[String] = {
    def parseStat(line:String):String = {
      line.split(":")(0)
    }

    object Mode extends Enumeration {
      type Mode = Value
      val Writes, Stats, Counts = Value
    }

    import Mode._
    var curMode = Writes
    var parsed = List[String]()


    try {
    for (line <- output){
      if (line.startsWith("- Counts")){
        curMode = Counts
      }
      curMode match {
        case Writes => parsed = parsed :+ line
        case Stats => 
        case Counts =>
      }
      if (line == "-------------------------"){
        curMode = Stats
      }
    }
    }
    catch {
      case _ : Throwable => return parsed
    }
    parsed
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
          if (groupNumber != -1){
            type_counts.put(groupNumber,typeCounts)
          }

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
    if (groupNumber != -1){
      type_counts.put(groupNumber,typeCounts)
    }
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

  def countDynamic(cfgs:List[CFG]):HashMap[Int,(Block, SIR with Instr,
    String, Int)] = {
    var counter_counter = 0
    val    dynamicCounters = new HashMap[Int,(Block, SIR with Instr,
    String, Int)]

    def addDynamicCounter(b:Block) {

      // Insert load, add, count on the target to determine type
      // Assumes loadTarget will always be a register due to the Start
      //front-end parsing
      def insertLdCounterWithRegister(loadTarget:Register,
      currentInstr:SIR with Instr, fieldName:String): List[SIR with Instr] = {
        val load = Load(loadTarget, IntType)
        val shifted_counter = counter_counter << 16
        val add = Add(Register(currentInstr.num), Immediate(shifted_counter),
      IntType)
        val count = Count(Register(currentInstr.num))

        dynamicCounters.put(counter_counter, (b, count,
        fieldName.split("_offset")(0), 0))

        counter_counter += 1
        val result = List(load, add, count)
        result
      }

      def insertLdCounter(target:Operand, offset:Operand, currentInstr:SIR with Instr): List[SIR with Instr] = {
        target match {
          case r:Register => offset match {
            case o:Offset => 
              insertLdCounterWithRegister(r,currentInstr,
        o.s);
            case _ => List()
          }
          case _ => List()
        }
      }


      def insertStCounterWithRegister(loadTarget:Register,
      currentInstr:SIR with Instr, fieldName:String): List[SIR with Instr] = {
        val load = Load(loadTarget, IntType)
        val shifted_counter = counter_counter << 16
        val add = Add(Register(currentInstr.num), Immediate(shifted_counter),
      IntType)
        val count = Count(Register(currentInstr.num))

        dynamicCounters.put(counter_counter, (b, count,
        fieldName.split("_offset")(0), 1))

        counter_counter += 1
        val result = List(load, add, count)
        result
      }
      def insertStCounter(source:Operand, target:Operand, offset:Option[Operand], currentInstr:SIR with Instr): List[SIR with Instr] = {
        target match {
          case r:Register => offset.get match {
            case o:Offset => 
              insertStCounterWithRegister(r,currentInstr,
        o.s);
            case _ => List()
          }
          case _ => List()
        }
      }

      var start:List[SIR with Instr] = List()
      var end = b.instrs.last

      val iter = b.instrs.iterator

      while (iter.hasNext){
        val current = iter.next()
        // if current is a dynamic load, insert the type counter
        //instructions in front of it
        current match {
          case ld:Lddynamic => 
            start = start ++ insertLdCounter(ld.a, ld.b, ld)
          case sd:Stdynamic => 
            start = start ++ insertStCounter(sd.a, sd.b, sd.c, sd)
          case _ =>
        }

        // shift this whole complex along the block...
        start = start :+ current
      }


      b.instrs = new ListBuffer[SIR with Instr]() ++ start

    }

    cfgs.foreach(_.list.foreach(addDynamicCounter))
    dynamicCounters
  }

}
