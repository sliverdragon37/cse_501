import scala.io.Source
import java.io.FileOutputStream
import java.io.PrintStream

object main {

  def main(args:Array[String]) {

    //first arg input file, second output
    val infname = args(0)
    val inlines = Source.fromFile(infname).getLines.toList

    val I = inlines.dropRight(1)
    val IR = I.map(SIRParser.runParser)


    //IR.foreach(println(_.toString))

    val CFGs = CFGFactory.makeAllCFGs(IR)


    //start time
    val start = System.nanoTime
    CFGs.foreach(cfg => dom.find_dominator(cfg))

    //end time
    val end = System.nanoTime

    System.err.println("Finding dominators took " + (end-start) + " nanoseconds")

    CFGs.foreach(cfg => println(cfg))
  }
}
