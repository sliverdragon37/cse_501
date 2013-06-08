import scala.io.Source
import java.io.FileOutputStream
import java.io.PrintStream
import scala.collection.mutable._
import scala.sys.process._

object main {

  def main(args:Array[String]) {

    //first arg input file name, second output file name
    val infname = args(0)
    val inlines = Source.fromFile(infname).getLines.toList

    val outfname = args(1)

    val I = inlines.dropRight(1)

    var branch_counters:HashMap[(Block,Block),Int] = null
    var branch_counts:HashMap[Int,Int] = null

    var ssa = false
    var cse = false
    var scp = false
    var cbr = false
    var mem = false
    var cnt:Int = 1
    while (cnt < args.length){
      if (args(cnt).takeRight(3) == "ssa"){
        ssa = true
      }

      if (args(cnt).takeRight(3) == "cse"){
        cse = true
      }

      if (args(cnt).takeRight(3) == "scp"){
        scp = true
      }
      if (args(cnt).takeRight(3) == "cbr"){
        cbr = true
      }
      if (args(cnt).takeRight(3) == "mem"){
        mem = true
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

    //CFGs.foreach(_.dumpGraphViz(outfname + "_preopt"))

    if (ssa){
      //convert each CFG to SSA
      CFGs.foreach(_.toSSA)

      //Run all optimizations that require SSA
      if (scp){
        CFGs.foreach(cprop.runCprop(_))
      }
      if (cse){
        CFGs.foreach(valuenumbering.runValnum(_))
      }
      if (cbr){
        branch_counters = profile.instrBranches(CFGs)
      }

      //convert back out of SSA
      CFGs.foreach(_.fromSSA)

    }

    //run cleanup peephole optimizations
    CFGs.foreach(_.peephole)

    //renumber instructions
    //(part of translation out of SSA)
    var i = 1
    var m:HashMap[Int,Int] = new HashMap[Int,Int]()
    for (c <- CFGs) {
      i = c.renumber(i,m)
    }

    CFGs.foreach(_.reRegister(m))

    //CFGs.foreach(_.dumpGraphViz(outfname + "_postopt"))

    val tHeaders = headers collect { case t:TypeDeclaration => t }
    val gHeaders = headers collect { case g:GlobalDeclaration => g }
    val mHeaders = CFGs.map(_.getHeader)
    val allHeaders = tHeaders ++ mHeaders ++ gHeaders

    val instrsOpt = (allHeaders.map(_.toString).map(_ + "\n")) ++ (CFGs.flatMap(_.list.flatMap(_.instrs)).map(_.toString).map(_+"\n"))

    //write the optimized code back out to file
    util.writeToFile(instrsOpt,outfname)

    if (cbr) {

      //get the output of running the program once
      val prof = (Seq("dart", "../../../start/bin/start.dart", "-r", "--stats", outfname)).lines

      //get branch counts from the output
      branch_counts = profile.getBranchCounts(prof)

      //tell each block what its branch counts were
      for (x <- branch_counters.keys) {
        val ctr = branch_counters(x)
        val count = branch_counts(ctr)
        x._1.setBias(x._2,count)
      }

      //remove count instructions, insert fallback branches
      profile.cleanUpCounters(CFGs)

      //renumber
      var i = 1
      var m:HashMap[Int,Int] = new HashMap[Int,Int]()
      for (c <- CFGs) {
        i = c.wrenumber(i,m)
      }

      CFGs.foreach(_.reRegister(m))

      //CFGs.foreach(_.dumpGraphViz(outfname + "_postprof"))

      //put program together to write out
      val profInstrsOpt = (allHeaders.map(_.toString).map(_ + "\n")) ++ (CFGs.flatMap(_.wlist.flatMap(_.instrs)).map(_.toString).map(_+"\n"))

      //write out our better optimized version
      util.writeToFile(profInstrsOpt,outfname)

    }

    if (mem) {

      //run program, get output
      val output = (Seq("dart", "../../../start/bin/start.dart", "-r", outfname)).lines

      val optProg = memoize.makeEquivProg(output)

      util.writeToFile(optProg,outfname)

    }
  }
}
