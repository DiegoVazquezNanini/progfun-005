object pack {

  //pack(List("a", "a", "a", "b", "c", "c", "a"))
  //List(List("a", "a", "a"), List("b"), List("c", "c"), List("a")).

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil      => Nil
    case x :: xs1 => val (first, rest) = (xs span (y => y == x))
      first :: pack(rest)
    }

  def bla[T](xs: List[List[T]]): List[(T, Int)] = xs match {
    case Nil      => Nil
    case x :: xs1 => (x.head, x.length) :: bla(xs1)
  }

  def encode_mo[T](xs: List[T]): List[(T, Int)] = pack(xs) map (ys => (ys.head, ys.length))

  def encode_dmv[T](xs: List[T]): List[(T, Int)] = bla(pack(xs))

  //List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))
  //List("a", "a", "a")
  //encode(List("a", "a", "a", "b", "c", "c", "a"))
  //encode(List("b"), List("c", "c"), List("a"))
  //List[(Any, Int)] = List((a,3), (List(b),1), (List(List(c, c)),1), (List(List(List(a))),1))

}




