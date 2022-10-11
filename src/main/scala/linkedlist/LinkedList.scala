package linkedlist

import optional.Optional

import scala.annotation.tailrec

sealed trait LinkedList[+A] {
  def head: A
  def headOption: Optional[A]
  def isEmpty: Boolean
  def tail: LinkedList[A]
  def foreach(f: A => Unit): Unit
  def append[A1 >: A](elem: A1): LinkedList[A1]
  def size: Int
  def contains[A1 >: A](elem: A1): Boolean
  def map[B](f: A => B): LinkedList[B]
  def flatMap[B](f: A => LinkedList[B]): LinkedList[B]
  def filter(f: A => Boolean): LinkedList[A]
  def withFilter(f: A => Boolean): LinkedList[A]
  def isDefinedAt(index: Int): Boolean
  def dropWhile(f: A => Boolean): LinkedList[A]
  def drop(amount: Int): LinkedList[A]
  def zip[B](that: LinkedList[B]): LinkedList[(A, B)]
  def distinct: LinkedList[A]
  def count(f: A => Boolean): Int
  def distinctBy[B](f: A => B): LinkedList[A]
  def take(amount: Int): LinkedList[A]
  def takeWhile(f: A => Boolean): LinkedList[A]
  def splitAt(index: Int): (LinkedList[A], LinkedList[A])
  def exists(f: A => Boolean): Boolean
  def find(f: A => Boolean): Optional[A]
  def concat[A1 >: A](that: LinkedList[A1]): LinkedList[A]
}

object LinkedList {
  
  def apply[A](elems: A*): LinkedList[A] = {
    @tailrec
    def loop(elements: Seq[A], accumulated: LinkedList[A]): LinkedList[A] = {
      if (elements.isEmpty) accumulated
      else loop(elements.tail, NonEmpty(elements.head, accumulated))
    }

    loop(elems.reverse, empty[A])
  }

  def nonEmpty[A](head: A, tail: LinkedList[A] = Empty): LinkedList[A] = NonEmpty(head, tail)

  def empty[A]: LinkedList[A] = Empty

  final case class NonEmpty[+A](override val head: A, override val tail: LinkedList[A]) extends LinkedList[A] {

    override def foreach(f: A => Unit): Unit = {
      f(head)
      tail.foreach(f)
    }

    override def append[A1 >: A](elem: A1): LinkedList[A1] = LinkedList.nonEmpty(elem, this)

    override def size: Int = 1 + tail.size

    override def contains[A1 >: A](elem: A1): Boolean = head == elem || tail.contains(elem)

    override def map[B](f: A => B): LinkedList[B] = LinkedList.nonEmpty(f(head), tail.map(f))

    override def flatMap[B](f: A => LinkedList[B]): LinkedList[B] = f(head)

    override def filter(f: A => Boolean): LinkedList[A] = if (f(head)) LinkedList.nonEmpty(head, tail.filter(f)) else tail.filter(f)

    override def withFilter(f: A => Boolean): LinkedList[A] = filter(f)

    override def toString: String = {
      @tailrec
      def loop(list: LinkedList[A], acc: String): String = {
        list match {
          case NonEmpty(head, Empty) => acc.concat(head.toString).concat(")")
          case NonEmpty(head, tail)  => loop(tail, acc.concat(head.toString).concat(", "))
        }
      }

      loop(this, "LinkedList(")
    }
  }

  case object Empty extends LinkedList[Nothing] {

    override def head: Nothing = throw new NoSuchElementException("Empty list.")

    override def tail: LinkedList[Nothing] = this

    override def foreach(f: Nothing => Unit): Unit = ()

    override def append[A1 >: Nothing](elem: A1): LinkedList[A1] = this

    override def size: Int = 0

    override def contains[A1 >: Nothing](elem: A1): Boolean = false

    override def map[B](f: Nothing => B): LinkedList[B] = this

    override def flatMap[B](f: Nothing => LinkedList[B]): LinkedList[B] = this

    override def filter(f: Nothing => Boolean): LinkedList[Nothing] = this

    override def withFilter(f: Nothing => Boolean): LinkedList[Nothing] = this

    override def toString: String = "LinkedList()"

  }

}
