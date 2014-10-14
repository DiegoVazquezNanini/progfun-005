package week4

object Lista {

  def isort(xs:List[Int]}): List[Int] = xs match {
    case List() => List()
    case y::ys  => insert(y,isort(ys))
  }

  def insert(x:Int, xs:List[Int]): List[Int] = xs match {
    case List() => List(x)
    case List(xs) => if (x <= y ) x::xs else y:: insert(x,ys)
  }

}

def remoteAt[T](n: Int, xs:List[t]):List[t] = (xs take n) ::: (xs drop n +1)

def flatten(xs: List[Any]): List[Any] = xs match {
  case x => x
  case List() =>
  case List => flatten()
}


flatten(List(List(1, 1), 2, List(3, List(5, 8))))
res0: List[Any] = List(1, 1, 2, 3, 5, 8)