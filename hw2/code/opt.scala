import scala.io.Source
import java.io.FileOutputStream
import java.io.PrintStream
import scala.collection.mutable._

object main {

  def main(args:Array[String]) {

        //first arg input file name, second output file name
    val infname = args(0)
    val inlines = Source.fromFile(infname).getLines.toList

    val outfname = args(1)

    val I = inlines.dropRight(1)


    var ssa = false
    var sce = false
    var scp = false
    var cnt:Int = 2
    while (cnt < args.length){
      if (args(cnt) == "ssa"){
        ssa = true
      }

      if (args(cnt) == "sce"){
        sce = true
      }

      if (args(cnt) == "scp"){
        scp = true
      }
      cnt+=1
    }


    //parse intermediate representation
    val IR = I.map(SIRParser.runParser)

    //get just the headers
    val headers = util.toRight(IR)

    //get just the instructions
    val instrs = util.toLeft(IR)

    //store global instr count
    util.instr_count = instrs.length

    //each CFG is a control flow graph for a method
    //made up of a list of basic blocks
    val CFGs = CFGFactory.makeAllCFGs(headers,instrs)

    //find dominators
    CFGs.foreach(cfg => dom.find_dominator(cfg))

    //finalize cfg construction
    //moves CFGs from start,end representation to
    //List[Instr] notation
    CFGs.foreach(_.finishConstruction)

    if (ssa){
      //convert each CFG to SSA
      CFGs.foreach(_.toSSA)

      //Run all optimizations that require SSA
      if (scp){
        CFGs.foreach(cprop.runCprop(_))
      }
      if (sce){
        CFGs.foreach(valnum.runValnum(_))
      }

      //convert back out of SSA
      CFGs.foreach(_.fromSSA)
    }

    //renumber instructions
    var i = 1
    var m:HashMap[Int,Int] = new HashMap[Int,Int]()
    for (c <- CFGs) {
      i = c.renumber(i,m)
    }

    val tHeaders = headers collect { case t:TypeDeclaration => t }
    val gHeaders = headers collect { case g:GlobalDeclaration => g }
    val mHeaders = CFGs.map(_.getHeader)
    val allHeaders = tHeaders ++ mHeaders ++ gHeaders

    //write the optimized code back out to file
    val out_file = new java.io.FileOutputStream(outfname)
    val out_stream = new java.io.PrintStream(out_file)
    val instrsOpt = (allHeaders.map(_.toString).map(_ + "\n")) ++ (CFGs.flatMap(_.list.flatMap(_.instrs)).map(_.toString).map(_+"\n"))
    instrsOpt.foreach(out_stream.print(_))
    out_stream.close
  }
}
