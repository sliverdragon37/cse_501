import scala.collection.mutable._
import scala.collection.immutable

sealed trait LatticeValue {}

case object Top extends LatticeValue
case object Bottom extends LatticeValue
case object Const extends LatticeValue

// Class for an expression for cprop purposes
class Expr (var latVal:LatticeValue, var sourceVal:Int, var instr:Instr){
  var constVal = 0;

  override def toString = {
    instr.toString + " : " + sourceVal + " : " + constVal + " : " +
      latVal + "\n"
  }
}

object cprop {

  def runCprop(cfg:CFG): List[Expr] = {
    var exprMap:HashMap[Int, Expr] = new HashMap[Int, Expr]()
    var ssaEdges:HashMap[Expr, Set[Expr]] = new HashMap[Expr, Set[Expr]]()
    var workList:Queue[(Expr,Expr)] = new Queue[(Expr, Expr)]()
    var instrList:ListBuffer[SSA] = new ListBuffer[SSA]()
    var exprList:ListBuffer[Expr] = new ListBuffer[Expr]()
    cfg.list.foreach(c => {println(c.instrsSSA.size); c.instrsSSA.foreach(ins => instrList +=
    ins)})

    exprList = instrList.map(i => ssaToExpr(i))
    println(exprList.size)
    exprList.foreach(e => exprMap.put(e.sourceVal, e))
    println(exprMap.size)
    println("+++ EXPRESSION MAP +++")
    exprMap.foreach(e => println(e))
    println("+++\t\t+++")
    ssaEdges = constructSsaEdges(exprMap)
    println("*** SSA EDGES ***")
    ssaEdges.foreach(s => println("" + s._1 + " : " + s._2))
    println("***\t\t***")
    workList = constructWorkList(ssaEdges)


    // propagate
    while (!workList.isEmpty){
      var work = workList.dequeue()

      if (work._2.latVal != Bottom){
        // Just need to look at source and target of this edge
        var temp = eval(work._1, work._2)
        if (temp != work._2.latVal){
          work._2.latVal = temp
          if (work._2.latVal == Const){
//            println("UPDATING: " + work._2)
            work._2.constVal = updateConstVal(work._2, exprMap)
//            println("UPDATED: " + work._2)
          }
          // add children to worklist
          workList = addToWorkList(work._2, ssaEdges, workList)
        }
      }
    }


    exprList.toList
  }


  // Add the pairs of (expr, expr2) for each expr2 in the set of
  //ssaEdges corresponding to expr
  // ssaEdges is mapping an expr to all the expr's that use it as an
  //operand, so we will find all such expr's and add those pairs to
  //the worklist
  def addToWorkList(expr:Expr, ssaEdges:Map[Expr, Set[Expr]], workList:Queue[(Expr,Expr)]): Queue[(Expr, Expr)] = {
    var result:Queue[(Expr, Expr)] = workList
    if (ssaEdges.contains(expr)){
      ssaEdges(expr).foreach(e => result.enqueue((expr, e)))
    }
    result
  }

  // Calculate the new constant value based on the instruction types
  //and the known operand constants
  def updateConstVal(expr:Expr, exprMap:Map[Int, Expr]): Int = {
    val result:Int = expr.instr match{
      case singleOp:Op => {
        val sourceVal = singleOp.a match{
          case i:Immediate => i.n
          case r:Register => exprMap(r.n).constVal
          case s:SSALocalVar => exprMap(s.hashCode()).constVal
          case _ => 0
        }
        expr.instr match{
          case _ => 0
        }
      }
      case singleOpt:Opt => {
        val sourceVal = singleOpt.a match{
          case i:Immediate => i.n
          case r:Register => exprMap(r.n).constVal
          case s:SSALocalVar => exprMap(s.hashCode()).constVal
          case _ => 0
        }
        expr.instr match{
          case _:Neg => -sourceVal
          case _ => 0
        }
      }
      case doubleOp:Opop => {
        val op1Val = doubleOp.a match{
          case i:Immediate => i.n
          case r:Register => exprMap(r.n).constVal
          case s:SSALocalVar => exprMap(s.hashCode()).constVal
          case _ => 0
        }
        val op2Val = doubleOp.b match{
          case i:Immediate => i.n
          case r:Register => exprMap(r.n).constVal
          case s:SSALocalVar => exprMap(s.hashCode()).constVal
          case _ => 0
        }
        expr.instr match{
          case _:Move => op1Val
          case _ => 0
        }
      }
      case doubleOpt:Opopt => {
        val op1Val = doubleOpt.a match{
          case i:Immediate => i.n
          case r:Register => exprMap(r.n).constVal
          case s:SSALocalVar => exprMap(s.hashCode()).constVal
          case _ => 0
        }
        val op2Val = doubleOpt.b match{
          case i:Immediate => i.n
          case r:Register => exprMap(r.n).constVal
          case s:SSALocalVar => exprMap(s.hashCode()).constVal
          case _ => 0
        }
        expr.instr match{
          case _:Add => op1Val + op2Val
          case _:Sub => op1Val - op2Val
          case _:Mul => op1Val * op2Val
          case _:Div => op1Val / op2Val
          case _:Mod => op1Val % op2Val
          case _:Cmpeq => if (op1Val == op2Val) 1 else 0
          case _:Cmple => if (op1Val <= op2Val) 1 else 0
          case _:Cmplt => if (op1Val < op2Val) 1 else 0
          case _ => 0
        }
      }
      case phi:Phi => expr.constVal

    }
    result
  }

  def eval(source:Expr, target:Expr): LatticeValue = {
    val result = source.latVal match{
      case Bottom => Bottom
      case Const => {
        target.instr match{
          case _:Phi => {
            if (source.constVal == target.constVal) Const 
            else Bottom
          }
          case _ => Const
        }
      }
      // This should never happen
      case Top => target.latVal
    }
    result
  }


  // Adds all edges whose source (1) expression is not of type Top
  def constructWorkList(edges:HashMap[Expr, Set[Expr]]): Queue[(Expr,Expr)] = {
    var workList:Queue[(Expr, Expr)] = new Queue[(Expr, Expr)]()

    edges.foreach(s => 
      if (s._1.latVal != (Top)) {
        s._2.foreach(e => workList.enqueue((s._1, e)))
      })

    workList
  }

  // Takes a hashmap, a key, and a value, and inserts the value into
  //the set paired with the key, creating a new set for the key if necessary
  def addToMappedSet[A,B](hm:HashMap[A, Set[B]], key:A, value:B):HashMap[A, Set[B]]
  = {
    var result:HashMap[A, Set[B]] = hm
      hm.get(key) match{
      case Some(_) => result(key).add(value)
      case None =>{
        var s = new HashSet[B]()
        s.add(value)
        result.put(key, s)
      }
    }
    result
  }

  // Construct the edges as a mapping of [ExprA, Set[ExprB, ExprC...]] such that A is
  //the expression where a value is defined which is used as an
  //operand in B and C and ...
  // If B is Bottom already we do not bother creating an edge, since B
  //cannot be changed by any influence from A.
  // If ExprB is a Move we have a special case to ensure we do not
  //create self-edges based on a local var being defined and used as
  //an operand
  def constructSsaEdges(exprMap:Map[Int, Expr]): HashMap[Expr, Set[Expr]] =
  {
    var result:HashMap[Expr,Set[Expr]] = new HashMap[Expr,Set[Expr]]()
    exprMap.foreach(e => {
      if (e._2.latVal != Bottom){
        e._2.instr match {
          case move:Move => 
            move.a match{
              case i:Immediate => // do nothing
              case r:Register => addToMappedSet(result, exprMap(r.n), e._2)
              case s:SSALocalVar => addToMappedSet(result,
  exprMap(s.hashCode()), e._2)
              // Nothing besides a register or an SSALocalVar could
              //wind up being a constant, so set this expression to
              //bottom premptively if it has any other operand
              case _ => e._2.latVal = Bottom
            }
          case singleOp:Op =>
            singleOp.a match{
              case i:Immediate => // do nothing
              case r:Register => addToMappedSet(result, exprMap(r.n), e._2)
              case s:SSALocalVar => addToMappedSet(result,
  exprMap(s.hashCode()), e._2)
              // Nothing besides a register or an SSALocalVar could
              //wind up being a constant, so set this expression to
              //bottom premptively if it has any other operand
              case _ => e._2.latVal = Bottom
            }
          case singleOpt:Opt =>

            singleOpt.a match{
              case i:Immediate => // do nothing
              case r:Register => addToMappedSet(result, exprMap(r.n), e._2)
              case s:SSALocalVar => addToMappedSet(result,
  exprMap(s.hashCode()), e._2)
              // Nothing besides a register or an SSALocalVar could
              //wind up being a constant, so set this expression to
              //bottom premptively if it has any other operand
              case _ => e._2.latVal = Bottom
            }
          case doubleOp:Opop => {
            println("DOU: " + doubleOp)
            doubleOp.a match{
              case i:Immediate => // do nothing
              case r:Register => addToMappedSet(result, exprMap(r.n), e._2)
              case s:SSALocalVar => addToMappedSet(result,
  exprMap(s.hashCode()), e._2)
              // Nothing besides a register or an SSALocalVar could
              //wind up being a constant, so set this expression to
              //bottom premptively if it has any other operand
              case _ => e._2.latVal = Bottom
            }
            doubleOp.b match{
              case i:Immediate => // do nothing
              case r:Register => addToMappedSet(result, exprMap(r.n), e._2)
              case s:SSALocalVar => addToMappedSet(result,
  exprMap(s.hashCode()), e._2)
              // Nothing besides a register or an SSALocalVar could
              //wind up being a constant, so set this expression to
              //bottom premptively if it has any other operand
              case _ => e._2.latVal = Bottom
            }
          }
          case doubleOpt:Opopt => {
            doubleOpt.a match{
              case i:Immediate => // do nothing
              case r:Register => addToMappedSet(result, exprMap(r.n), e._2)
              case s:SSALocalVar =>
                addToMappedSet(result,exprMap(s.hashCode()), e._2)
              // Nothing besides a register or an SSALocalVar could
              //wind up being a constant, so set this expression to
              //bottom premptively if it has any other operand
              case _ => e._2.latVal = Bottom
            }
            doubleOpt.b match{
              case i:Immediate => // do nothing
              case r:Register => addToMappedSet(result, exprMap(r.n), e._2)
              case s:SSALocalVar => addToMappedSet(result,
  exprMap(s.hashCode()), e._2)
              // Nothing besides a register or an SSALocalVar could
              //wind up being a constant, so set this expression to
              //bottom premptively if it has any other operand
              case _ => e._2.latVal = Bottom
            }
          }
          case phi:Phi => {
            phi.args.foreach(arg => {arg._2 match {
                case DEAD =>
                case _ => addToMappedSet(result,
                  exprMap(arg._2.hashCode()), e._2)
              }
            })
          }
        }
      }
    })
    result
  }

    def ssaToExpr(instr:SSA):Expr = {
      val expr = instr match{
        case a:Enter => new Expr(Bottom, a.num, a)
        case Entrypc => new Expr(Bottom, -1, Entrypc)
        case a:Br => new Expr(Bottom, a.num, a)
        case a:Blbc => new Expr(Bottom, a.num, a)
        case a:Blbs => new Expr(Bottom, a.num, a)
        case a:Call => new Expr(Bottom, a.num, a)
        case a:Ret => new Expr(Bottom, a.num, a)
        case a:Nop => new Expr(Bottom, a.num, a)
        case a:Add => new Expr(initialEval(a), a.num, a)
        case a:Sub => new Expr(initialEval(a), a.num, a)
        case a:Mul => new Expr(initialEval(a), a.num, a)
        case a:Div => new Expr(initialEval(a), a.num, a)
        case a:Mod => new Expr(initialEval(a), a.num, a)
        case a:Neg => new Expr(initialEval(a), a.num, a)
        case a:Cmpeq => new Expr(initialEval(a), a.num, a)
        case a:Cmple => new Expr(initialEval(a), a.num, a)
        case a:Cmplt => new Expr(initialEval(a), a.num, a)
        case a:Isnull => new Expr(Bottom, a.num, a)
        case a:Istype => new Expr(Bottom, a.num, a)
        case a:Load => new Expr(Bottom, a.num, a)
        case a:Store => new Expr(initialEval(a), a.num, a)
        case a:Move => new Expr(initialMoveEval(a), getMoveSource(a), a)
        case a:New => new Expr(Bottom, a.num, a)
        case a:Newlist => new Expr(Bottom, a.num, a)
        case a:Checknull => new Expr(Bottom, a.num, a)
        case a:Checktype => new Expr(Bottom, a.num, a)
        case a:Checkbounds => new Expr(Bottom, a.num, a)
        case a:Lddynamic => new Expr(Bottom, a.num, a)
        case a:Stdynamic => new Expr(initialEval(a), a.num, a)
        case a:Write => new Expr(Bottom, a.num, a)
        case a:Wrl => new Expr(Bottom, a.num, a)
        case a:Param => new Expr(Bottom, a.num, a)
        case a:Phi => new Expr(Top, getPhiSource(a), a)
      }
      if (expr.latVal == Const){
        expr.constVal = getConstVal(instr)
      }
      expr
    }


    // Should only be this subset of instructions that can make to to
    //this method.
    // Any instruction reaching this method should only have immediates
    //for operands
    // Some instructions can be constant, but cannot propagate that
    //constant value anywhere (branches, returns, calls). These just
    //get a value of 0 for consistency but that value should never
    //matter. These might be able to always be set to Bottom,
    //actually...
    // I'm also too lazy to figure out the proper way to bring in the
    //instr argument such that the method knows the operands are
    //Immediates, so lots of .asInstanceOf
    def getConstVal(instr:SSA): Int ={
      val value = instr match {
        case k:Br => 0
        case k:Blbc => 0
        case k:Blbs => 0
        case k:Call => 0
        case k:Ret => 0
        case k:Add => k.a.asInstanceOf[Immediate].n + k.b.asInstanceOf[Immediate].n
        case k:Sub => k.a.asInstanceOf[Immediate].n - k.b.asInstanceOf[Immediate].n
        case k:Mul => k.a.asInstanceOf[Immediate].n * k.b.asInstanceOf[Immediate].n
        case k:Div => k.a.asInstanceOf[Immediate].n / k.b.asInstanceOf[Immediate].n
        case k:Mod => k.a.asInstanceOf[Immediate].n % k.b.asInstanceOf[Immediate].n
        case k:Neg => -k.a.asInstanceOf[Immediate].n
        case k:Cmpeq => if (k.a.asInstanceOf[Immediate].n == k.b.asInstanceOf[Immediate].n) 1 else 0
        case k:Cmple => if (k.a.asInstanceOf[Immediate].n <= k.b.asInstanceOf[Immediate].n) 1 else 0
        case k:Cmplt => if (k.a.asInstanceOf[Immediate].n < k.b.asInstanceOf[Immediate].n) 1 else 0
        case k:Move => k.a.asInstanceOf[Immediate].n
        case k:New => 0
        case k:Newlist => 0
        case k:Stdynamic => k.a.asInstanceOf[Immediate].n
        case k:Write => 0
        case k:Wrl => 0
        case k:Param => 0
        case _ => 0
      }
      value
    }

    def initialMoveEval(move:Move): LatticeValue ={
      val latVal = move.a match {
        case _:Immediate => Const
        case _:Register => Top
        case _:SSALocalVar => Top
        case _ => Bottom
      }
      latVal
    }

    // If all the operands are immediates, return Const.
    // If one or more operands are parameters, return Bottom
    // Otherwise return Top
    def initialEval(instr:Instr): LatticeValue = {
      val latVal = instr match {
        case singleOp:Op => {
          singleOp.a match{
            case _:Immediate => Const
            case _:Register => Top
            case _:SSALocalVar => Top
            case _ => Bottom
          }
        }
        case singleOpt:Opt => {
          singleOpt.a match{
            case _:Immediate => Const
            case _:Register => Top
            case _:SSALocalVar => Top
            case _ => Bottom
          }
        }case doubleOp:Opop => {
          doubleOp.a match{
            case _:Immediate => {
              doubleOp.b match{
                case _:Immediate => Const
                case _:Register => Top
                case _:SSALocalVar => Top
                case _ => Bottom
              }
            }
            case _:Register => {
              doubleOp.b match{
                case _:Immediate => Top
                case _:Register => Top
                case _:SSALocalVar => Top
                case _ => Bottom
              }
            }
            case _:SSALocalVar =>  {
              doubleOp.b match{
                case _:Immediate => Top
                case _:Register => Top
                case _:SSALocalVar => Top
                case _ => Bottom
              }
            }
            case _ => Bottom
          }
        }
        case doubleOpt:Opopt => {
          doubleOpt.a match{
            case _:Immediate => {
              doubleOpt.b match{
                case _:Immediate => Const
                case _:Register => Top
                case _:SSALocalVar => Top
                case _ => Bottom
              }
            }
            case _:Register =>{
              doubleOpt.b match{
                case _:Immediate => Top
                case _:Register => Top
                case _:SSALocalVar => Top
                case _ => Bottom
              }
            }
            case _:SSALocalVar =>  {
              doubleOpt.b match{
                case _:Immediate => Top
                case _:Register => Top
                case _:SSALocalVar => Top
                case _ => Bottom
              }
            }
            case _ => Bottom
          }
        }
        case _ => Bottom
      }
      latVal
    }

    // move.b ought to only ever be a local of the form "x$0" at this
    //point
    def getMoveSource(move:Move): Int = {
      move.b.hashCode()
    }

    // phi.a ought to only ever be a local of the form "x$0" at this
    //point
    def getPhiSource(phi:Phi): Int = {
      phi.ssa.hashCode()
    }



  }

