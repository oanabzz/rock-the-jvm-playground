package interview.lists

import java.util.NoSuchElementException
import scala.annotation.tailrec

sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def headOption: Option[T]
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  // kth element
  def apply(index: Int): T

  // length
  def length: Int

  // reverse a list
  def reverse: RList[T]

  // append a list
  def ++[S >: T](anotherList: RList[S]): RList[S]

  // remove an element from an index
  def removeAt(index: Int): RList[T]

  def map[S](f: T => S): RList[S]

  def flatMap[S](f: T => RList[S]): RList[S]

  def filter(f: T => Boolean): RList[T]
}

object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    @tailrec
    def fromTailrec(currentIterable: Iterable[T], result: RList[T]): RList[T] = {
      if(currentIterable.isEmpty) result
      else fromTailrec(currentIterable.tail, currentIterable.head :: result)
    }

    fromTailrec(iterable, RNil).reverse
  }
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException

  override def tail: RList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def headOption: Option[Nothing] = None

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException

  override def length: Int = 0

  override def reverse: RList[Nothing] = this

  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  override def removeAt(index: Int): RList[Nothing] = RNil

  override def map[S](f: Nothing => S): RList[S] = RNil

  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil

  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {  // constructor
  override def isEmpty: Boolean = false

  override def headOption: Option[T] = Some(head)

  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String = {
      if(remaining.isEmpty) result
      else if(remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
    }

    s"[${toStringTailrec(this, "")}]"
  }

  // O(n)
  override def apply(index: Int): T = {
    @tailrec
    def getElemTailrec(toFind: Int, list: RList[T]): T = {
      if(toFind == 0) list.head
      else getElemTailrec(toFind - 1, list.tail)
    }

    if(index < 0) throw new NoSuchElementException
    else getElemTailrec(index, this)
  }

  // O(n)
  override def length: Int = {
    @tailrec
    def lengthTailrec(currentLength: Int, currentList: RList[T]): Int = {
      if(currentList.isEmpty) currentLength
      else lengthTailrec(currentLength + 1, currentList.tail)
    }
    lengthTailrec(0, this)
  }

  override def reverse: RList[T] = {
    @tailrec
    def reverseTailrec(currentList: RList[T], acc: RList[T]): RList[T] = {
      if(currentList.isEmpty) acc
        // :: is right-associative <- figure this out :)
      else reverseTailrec(currentList.tail, currentList.head :: acc)
    }

    reverseTailrec(this, RNil)
  }

  // O(N + M)
  override def ++[S >: T](anotherList: RList[S]): RList[S] = {
    @tailrec
    def appendTailrec(currentList: RList[S], result: RList[S]): RList[S] = {
      if(currentList.isEmpty) result
      else appendTailrec(currentList.tail, currentList.head :: result)
    }

    appendTailrec(this.reverse, anotherList)
  }

  // O(N) but more like O(2N) because of the reverse
  // it's obv still O(N) since order and stuff, but would've been nice to explain :)
  override def removeAt(index: Int): RList[T] = {
    @tailrec
    def removeTailrec(currentIndex: Int, currentList: RList[T], acc: RList[T]): RList[T] = {
      if(currentList.isEmpty) acc.reverse
      else if(currentIndex == index) acc.reverse ++ currentList.tail
      else removeTailrec(currentIndex + 1, currentList.tail, currentList.head :: acc)
    }

    removeTailrec(0, this, RNil)
  }

  override def map[S](f: T => S): RList[S] = {
    @tailrec
    def mapTailrec(currentList: RList[T], acc: RList[S]): RList[S] = {
      if(currentList.isEmpty) acc.reverse
      else mapTailrec(currentList.tail, f(currentList.head) :: acc)
    }

    mapTailrec(this, RNil)
  }

  override def flatMap[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def flatMapTailrec(currentList: RList[T], acc: RList[S]): RList[S] = {
      if(currentList.isEmpty) acc
      else flatMapTailrec(currentList.tail, acc ++ f(currentList.head))
    }

    flatMapTailrec(this, RNil)
  }

  override def filter(f: T => Boolean): RList[T] = {
    @tailrec
    def filterTailrec(currentList: RList[T], acc: RList[T]): RList[T] = {
      if(currentList.isEmpty) acc.reverse
      else if(f(currentList.head)) filterTailrec(currentList.tail, acc)
      else filterTailrec(currentList.tail, currentList.head :: acc)
    }

    filterTailrec(this, RNil)
  }
}

object ListsProblems extends App {
  assert(RNil.::(2) == 2 :: RNil)

  val aSmallList = ::(1, ::(2, ::(3, RNil)))
  val aMediumList = RList.from(1 to 10)
  val aLargeList = RList.from(1 to 10000)

  println(aSmallList)

  assert(aSmallList.apply(0) == 1)
  assert(aSmallList.apply(1) == 2)
  assert(aSmallList.apply(2) == 3)

  assert(aSmallList.length == 3)
  assert(aSmallList.tail.length == 2)
  assert(aSmallList.tail.tail.length == 1)
  assert(aSmallList.tail.tail.tail.length == 0)

  assert(aSmallList.reverse == ::(3, ::(2, ::(1, RNil))))
  assert(RNil.reverse == RNil)

  println(RList.from(1 to 10))

  println(RList.from(1 to 5) ++ RList.from(6 to 10))

  println(aMediumList.removeAt(5))

  println(aMediumList.map(_ * 2))
  println(aMediumList.flatMap((elem: Int) => ::(elem, ::(elem * 2, RNil))))
  println(aMediumList.filter(_%2 ==0))
  println()
}
