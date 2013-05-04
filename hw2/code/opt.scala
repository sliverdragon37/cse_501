import scala.io.Source
import java.io.FileOutputStream
import java.io.PrintStream

object main {

  def main(args:Array[String]) {

    //first arg input file name, second output file name
    val infname = args(0)
    val inlines = Source.fromFile(infname).getLines.toList

    val outfname = args(1)

    val I = inlines.dropRight(1)

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

    //convert each CFG to SSA
    CFGs.foreach(_.toSSA)

    //print out the SSA representation
    CFGs.foreach(_.printSSA)

    //print the stats about CFGs
    //CFGs.foreach(System.err.println(_.getStats))
    //System.err.println("Finding dominators took " + (end-start) + " nanoseconds")

    //write the optimized code back out to file
    val out_file = new java.io.FileOutputStream(outfname)
    val out_stream = new java.io.PrintStream(out_file)
    (headers ++ instrs).map(_.toString).map(_+"\n").foreach(out_stream.print(_))
    out_stream.close
  }
}
