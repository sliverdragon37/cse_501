object util {

  def toLeft[A,B](l:List[Either[A,B]]):List[A] = l.flatMap({_ match {
    case Left(a) => List(a)
    case Right(_) => List()
  }})
  def toRight[A,B](l:List[Either[A,B]]):List[B] = l.flatMap({_ match {
    case Left(_) => List()
    case Right(b) => List(b)
  }})
}
