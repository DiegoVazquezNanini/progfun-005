package objsets

import java.util.NoSuchElementException

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

}
class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

//def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
//singleton[Int](1) or singleton(1) using type inference
//singleton[Boolean](true) or singleton(true) using type inference