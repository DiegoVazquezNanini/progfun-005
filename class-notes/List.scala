package objsets

import java.util.NoSuchElementException

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

}
class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

object Nil extends List[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object test {
  val x: List[String] = Nil
}

object List{
//  List(1,2)
  def apply[T](x1:T, x2:T, x3:T): List[T] = new Cons(x1,new Cons(x2, new Cons(x3, new Nil)))
  def apply[T](x1:T, x2:T): List[T] = new Cons(x1,new Cons(x2, new Nil))
  def apply[T](): List[T] = new Nil
}

//def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
//singleton[Int](1) or singleton(1) using type inference
//singleton[Boolean](true) or singleton(true) using type inference

//Peano Numbers
abstract class Nat {

  def isZero: Boolean
  def predecessor: Nat
  def succesor: Nat
  def + (that: Nat):Nat
  def - (that: Nat):Nat

}

object Nat {
  def
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Error("0.predecessor")
  def succesor = new succ(1)
  def + (that: Nat):Nat = that
  def - (that: Nat):Nat = if (that.isZero) this else throw new Error("negative number")
}

class Succ(n:Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def succesor = new Succ(this)
  def + (that: Nat):Nat = Succ(that + n)
  def - (that: Nat):Nat = if (that.isZero) this else n - that.predecessor
}