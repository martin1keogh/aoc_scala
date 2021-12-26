package commons

type Matrix[T] = List[List[T]]

def wrapMatrix[T](matrix: Matrix[T]): Matrix[Option[T]] = matrix match {
  case Nil => List(List(None))
  case row :: _ =>
    val inner = matrix.map(_.map(Some.apply))
    val wrappedRows = inner.map(row => None :: (row :+ None))
    val topBottom = List.fill(row.length + 2)(None)
    topBottom :: (wrappedRows :+ topBottom)
}
