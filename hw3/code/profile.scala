
object profile {
  def instrBranches(cfg:CFG) {
    def addBranchCounter(b:Block) {
      val body = b.instrsSSA.init

      val branch = b.instrsSSA.last 
      val instrBranch = branch match {
        case Br(a) => //write me
        case Blbc(a,b,c) =>
        case Blbs(a,b,c) =>
        case Ret(a) =>
      }
    }
    cfg.list.foreach(addBranchCounter)
  }
}
