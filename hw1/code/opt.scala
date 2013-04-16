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

    val outlines = IR.map(_.toString)

    outlines.foreach(println)

    var CFGs = CFGFactory.makeAllCFGs(IR)
    CFGs.foreach(cfg => dom.find_dominator(cfg))

    CFGs.foreach(cfg => println(cfg))
  }
}
