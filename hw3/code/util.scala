import scala.collection.mutable._
import scala.collection.GenTraversableOnce

object util {

  def toLeft[A,B](l:List[Either[A,B]]):List[A] = l.flatMap({_ match {
    case Left(a) => List(a)
    case Right(_) => List()
  }})
  def toRight[A,B](l:List[Either[A,B]]):List[B] = l.flatMap({_ match {
    case Left(_) => List()
    case Right(b) => List(b)
  }})
  def toMapSet[A,B](l:GenTraversableOnce[(A,B)]):HashMap[A,HashSet[B]] = {
    var m = new HashMap[A,HashSet[B]]()
    for ((a,b) <- l) {
      m.get(a) match {
        case Some(_) => m(a).add(b)
        case None => {
          var sb = new HashSet[B]()
          sb += b
          m += (a -> sb)
        }
      }
    }
    m
  }

  var instr_count = -1
}
