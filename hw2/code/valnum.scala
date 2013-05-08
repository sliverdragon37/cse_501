import scala.collection.immutable
import scala.collection.mutable._

//basic idea behind value numbering:
//forward pass through code in a function
//give the result of every computation a value number
//in such a way that if create a value number twice for the same computation
//we get the same number back
//thus, if we try to construct a computation for a given value
//that has already been computed, we simply copy the 
//already computed value instead

//specifically, the optimization below is the DVNT global value numbering
//optimization described by Briggs, Cooper and Simpson

object valnum {

  //sealed trait valop {}

  sealed trait Valbinop {}
  sealed trait Commutes extends Valbinop {}
  case object add extends Commutes
  case object sub extends Valbinop
  case object mul extends Commutes
  case object div extends Valbinop
  case object mod extends Valbinop
  case object cmpeq extends Commutes
  case object cmplt extends Valbinop
  case object cmple extends Valbinop
  case object istype extends Valbinop
  case object checktype extends Valbinop

  sealed trait Valunaryop {}
  case object isnull extends Valunaryop
  case object checknull extends Valunaryop

  type Bin = (Valbinop,ValNum,ValNum)
  type Un = (Valunaryop,ValNum)

  def runValnum(cfg:CFG) = {
    //available expressions go here
    val availBinExp:HashMap[Bin,ValNum] = new HashMap[Bin,ValNum]()
    val availUnExp:HashMap[Un,ValNum] = new HashMap[Un,ValNum]()

    //available values go here (registers and locals)
    val availVal:HashMap[Operand,ValNum] = new HashMap[Operand,ValNum]()


    def dvnt(b:Block):Unit = {

      val binLocal = new HashSet[Bin]()
      val unLocal = new HashSet[Un]()
      val valLocal = new HashSet[Operand]()

      def numberBinExp(i:Opopt with Instr,op:Valbinop):Unit = {
        //grab the two operands
        (i.a,i.b) match {
          case (v1:ValNum,v2:ValNum) => {
            availBinExp.get((op,v1,v2)) match {
              //if already has a number
              //op becomes dead, all subsequent uses of def
              //will use value num instead
              case Some(vnum) => {
                val r = Register(i.num)
                availVal.put(r,vnum)
                valLocal.add(r)
                i.live = false
              }
              //if doesn't have val num
              //see if it commutes
              case None => {
                op match {
                  case o:Commutes => {
                    availBinExp.get((op,v2,v1)) match {
                      case Some(vnum) => {
                        val r = Register(i.num)
                        availVal.put(r,vnum)
                        valLocal.add(r)
                        i.live = false
                      }
                      case None => {
                        val r = Register(i.num)
                        val vnum = ValNum(r)
                        availBinExp.put((op,v1,v2),vnum)
                        binLocal.add((op,v1,v2))
                        availVal.put(r,vnum)
                        valLocal.add(r)
                      }
                    }
                  }
                  case _ => {
                    val r = Register(i.num)
                    val vnum = ValNum(r)
                    availBinExp.put((op,v1,v2),vnum)
                    binLocal.add((op,v1,v2))
                    availVal.put(r,vnum)
                    valLocal.add(r)
                  }
                }
              }
            }
          }
          case _ => {
            //TODO: really do nothing here?
          }
        }
      }

      def numberUnaryExp(i:Opt with Instr,op:Valunaryop):Unit = {
        //grab the operand
        i.a match {
          case v:ValNum => {
            availUnExp.get((op,v)) match {
              case Some(vnum) => {
                val r = Register(i.num)
                availVal.put(r,vnum)
                valLocal.add(r)
                i.live = false
              }
              case None => {
                val r = Register(i.num)
                val vnum = ValNum(r)
                availUnExp.put((op,v),vnum)
                unLocal.add((op,v))
                availVal.put(r,vnum)
                valLocal.add(r)
              }
            }
          }
          case _ => {
            //TODO: really do nothing here?
          }
        }
      }


      //go through instructions
      def process(i:SSA) = {
        //replace all operands with value numbered ones
        def repl(o:Operand):Operand = {
          o match {
            case r:Register => availVal(r)
            case l:SSALocalVar => availVal(l)
            case _ => o
          }
        }
        i.opMap(repl)

        def getDistinctVal(p:Phi):Option[ValNum] = {
          val vnums = p.args.values.flatMap(availVal.get(_) match {
            case Some(vn) => List(Some(vn))
            case None => List()
          })
          vnums match {
            case List() => None
            case f::r => if (r.exists(_ != f)) {
              None
            } else {
              f
            }
          }
        }

        i match {
          case p:Phi => {
            getDistinctVal(p) match {
              //phi has more than one vnum
              //or is completely dead
              case None => {
                availVal.put(p.a,ValNum(p.a))
                valLocal.add(p.a)
              }
              //phi has exactly one vnum
              case Some(vnum) => {
                availVal.put(p.a,vnum)
                valLocal.add(p.a)
              }
            }
          }
          //deal with the rest of the instructions accordingly
          case i:Opopt => {
            i match {
              case in:Instr => {
                in match {
                  case a:Add => numberBinExp(in,add)
                  case m:Mul => numberBinExp(in,mul)
                  case d:Div => numberBinExp(in,div)
                  case s:Sub => numberBinExp(in,sub)
                  case m:Mod => numberBinExp(in,mod)
                  case c:Cmpeq => numberBinExp(in,cmpeq)
                  case c:Cmplt => numberBinExp(in,cmplt)
                  case c:Cmple => numberBinExp(in,cmple)
                  case i:Istype => numberBinExp(in,istype)
                  case c:Checktype => numberBinExp(in,checktype)
                  case _ => {}
                }
              }
            }
          }
          case ii:Opt => {
            ii match {
              case in:Instr => {
                in match {
                  case i:Isnull => numberUnaryExp(in,isnull)
                  case c:Checknull => numberUnaryExp(in,checknull)
                }
              }
            }
          }
          case _ => {
          }
        }
      }
      b.instrsSSA.foreach(process)

      //all phi function args will be dealt with in next block
      b.children.foreach(dvnt)

      //clean up vars defined in this scope
      valLocal.foreach(availVal.remove(_))
      binLocal.foreach(availBinExp.remove(_))
      unLocal.foreach(availUnExp.remove(_))
    }
  }
}