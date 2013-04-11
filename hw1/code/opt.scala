import scala.io.Source
import java.io.FileOutputStream
import java.io.PrintStream

object main {

  def parse(in:String):SIR {

  }

  def main(args:Array[String]) {

    //first arg input file, second output
    val infname = args(0)
    val inlines:Iterator[String] = Source.fromFile(infname).getLines

    val IR = inlines.map(parse)

    val outlines = optimize(IR).map(_.toString)
    outlines.foreach(println)
  }
}
