import scala.io.Source
import java.io.FileOutputStream
import java.io.PrintStream

object main {

  def main(args:Array[String]) {

    //first arg input file, second output
    val infname = args(0)
    val inlines = Source.fromFile(infname).getLines.toList

    val outfname = args(1)

    val I = inlines.dropRight(1)

    //intermediate representation
    val IR = I.map(SIRParser.runParser)

    //get just the instructions
    val headers = util.toRight(IR)

    //get just the headers
    val instrs = util.toLeft(IR)

    //each CFG is a control flow graph for a method
    //made up of a list of basic blocks
    val CFGs = CFGFactory.makeAllCFGs(IR)


    //time finding dominators
    //start time
    val start = System.nanoTime
    CFGs.foreach(cfg => dom.find_dominator(cfg))

    //end time
    val end = System.nanoTime


    //print the stats about CFGs
    //CFGs.foreach(System.err.println(_.getStats))

    //System.err.println("Finding dominators took " + (end-start) + " nanoseconds")

    val out_file = new java.io.FileOutputStream(outfname)
    val out_stream = new java.io.PrintStream(out_file)
    (headers ++ instrs).map(_.toString).map(_+"\n").foreach(out_stream.print(_))
    out_stream.close
  }
}
