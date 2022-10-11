package linkedlist

import optional.Optional

import scala.annotation.tailrec

sealed trait LinkedList[+A] {
  def apply(index: Int): A
  def head: A
  def headOption: Optional[A]
  def isEmpty: Boolean
  def last: A
  def tail: LinkedList[A]
  def init: LinkedList[A]
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
  def foldLeft[B](base: B)(reducer: (B, A) => B): B
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
  def concat[A1 >: A](that: LinkedList[A1]): LinkedList[A1]
  def dropLast: LinkedList[A]
  def reverse: LinkedList[A]
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

    override def apply(index: Int): A = {
      if (index < 0 || index >= this.size) throw new IllegalArgumentException("index must be within the length")
      @tailrec
      def loop(count: Int, elems: LinkedList[A]): A = {
        if (count == index) elems.head
        else loop(count + 1, elems.tail)
      }

      loop(0, this)
    }


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

    override def headOption: Optional[A] = Optional.present(head)

    override def isEmpty: Boolean = false

    override def isDefinedAt(index: Int): Boolean = {

      @tailrec
      def loop(currentIndex: Int, list: LinkedList[A]): Boolean = {
        if (currentIndex < 0) false
        else if (currentIndex == 0 && !list.isEmpty) true
        else loop(currentIndex - 1, list.tail)
      }

      loop(index, this)
    }

    override def dropWhile(f: A => Boolean): LinkedList[A] = {

      @tailrec
      def loop(originalList: LinkedList[A], accumulated: LinkedList[A]): LinkedList[A] = {
        originalList match {
          case NonEmpty(head, tail) => if (f(head)) loop(tail, accumulated) else loop(tail, LinkedList.nonEmpty(head, accumulated))
          case Empty => accumulated.reverse
        }
      }

      loop(this, Empty)
    }

    override def drop(amount: Int): LinkedList[A] = ???

    override def zip[B](that: LinkedList[B]): LinkedList[(A, B)] = ???

    override def distinct: LinkedList[A] = ???

    override def count(f: A => Boolean): Int = ???

    override def distinctBy[B](f: A => B): LinkedList[A] = ???

    override def take(amount: Int): LinkedList[A] = ???

    override def takeWhile(f: A => Boolean): LinkedList[A] = ???

    override def splitAt(index: Int): (LinkedList[A], LinkedList[A]) = {

      @tailrec
      def loop(idx: Int, original: LinkedList[A], left: LinkedList[A]): (LinkedList[A], LinkedList[A]) = {
        if (idx == index) (left.reverse, original)
        else loop(idx + 1, original.tail, left.append(original.head))
      }

      loop(0, this, LinkedList.empty[A])
    }

    override def exists(f: A => Boolean): Boolean = find(f).fold(false)(_ => true)

    override def find(f: A => Boolean): Optional[A] = {

      @tailrec
      def loop(list: LinkedList[A]): Optional[A] = list match {
        case NonEmpty(head, tail) => if (f(head)) Optional.present(head) else loop(tail)
        case Empty => Optional.absent[A]
      }

      loop(this)
    }

    override def concat[A1 >: A](that: LinkedList[A1]): LinkedList[A1] = {
      val firstReversed = this.reverse

      @tailrec
      def loop(original: LinkedList[A1], acc: LinkedList[A1]): LinkedList[A1] = original match {
        case NonEmpty(h, t) => loop(original.tail, acc.append(h))
        case _ => acc
      }

      loop(firstReversed, that)
    }

    override def last: A = this match {
      case NonEmpty(h, Empty) => h
      case NonEmpty(_, t)     => t.last
      case _                  => throw new NoSuchElementException("Empty")
    }

    override def init: LinkedList[A] = {

      lazy val length: Int = this.size
      lazy val startIndex: Int = length - 2

      @annotation.tailrec
      def loop(updatedIndex: Int, updatedList: LinkedList[A]): LinkedList[A] = updatedList match  {
        case Empty => loop(updatedIndex - 1, updatedList.append(this(updatedIndex)))
        case NonEmpty(_, Empty) => loop(updatedIndex - 1, updatedList.append(this (updatedIndex)))
        case NonEmpty(_, _) =>
          if (updatedIndex == 0) updatedList.append(this (updatedIndex))
          else loop(updatedIndex - 1, updatedList.append(this (updatedIndex)))
      }

      loop(startIndex, Empty)
    }

    override def dropLast: LinkedList[A] = {

      @tailrec
      def loop(original: LinkedList[A], acc: LinkedList[A]): LinkedList[A] = original match {
        case NonEmpty(h, t) if t != Empty => loop(t, acc = acc.append(h))
        case _ => acc.reverse
      }

      loop(this, LinkedList.empty[A])
    }

    override def foldLeft[B](base: B)(reducer: (B, A) => B): B = {

      @tailrec
      def loop(updated: LinkedList[A], acc: B): B = updated match {
        case Empty => acc
        case NonEmpty(_, _) => loop(updated.tail, reducer(acc, updated.head))
      }

      loop(this, base)
    }

    override def reverse: LinkedList[A] = foldLeft(LinkedList.empty[A])((acc, elem) => LinkedList.nonEmpty[A](elem, acc))
  }

  case object Empty extends LinkedList[Nothing] {

    override def head: Nothing = throw new NoSuchElementException("Empty list.")

    override def tail: LinkedList[Nothing] = this

    override def foreach(f: Nothing => Unit): Unit = ()

    override def append[A1 >: Nothing](elem: A1): LinkedList[A1] = LinkedList.nonEmpty(elem, this)

    override def size: Int = 0

    override def contains[A1 >: Nothing](elem: A1): Boolean = false

    override def map[B](f: Nothing => B): LinkedList[B] = this

    override def flatMap[B](f: Nothing => LinkedList[B]): LinkedList[B] = this

    override def filter(f: Nothing => Boolean): LinkedList[Nothing] = this

    override def withFilter(f: Nothing => Boolean): LinkedList[Nothing] = this

    override def toString: String = "LinkedList()"

    override def headOption: Optional[Nothing] = ???

    override def isEmpty: Boolean = true

    override def isDefinedAt(index: Int): Boolean = false

    override def dropWhile(f: Nothing => Boolean): LinkedList[Nothing] = ???

    override def drop(amount: Int): LinkedList[Nothing] = ???

    override def zip[B](that: LinkedList[B]): LinkedList[(Nothing, B)] = ???

    override def distinct: LinkedList[Nothing] = ???

    override def count(f: Nothing => Boolean): Int = ???

    override def distinctBy[B](f: Nothing => B): LinkedList[Nothing] = ???

    override def take(amount: Int): LinkedList[Nothing] = ???

    override def takeWhile(f: Nothing => Boolean): LinkedList[Nothing] = ???

    override def splitAt(index: Int): (LinkedList[Nothing], LinkedList[Nothing]) = ???

    override def exists(f: Nothing => Boolean): Boolean = ???

    override def find(f: Nothing => Boolean): Optional[Nothing] = Optional.absent

    override def concat[A1 >: Nothing](that: LinkedList[A1]): LinkedList[A1] = that

    override def last: Nothing = ???

    override def init: LinkedList[Nothing] = ???

    override def dropLast: LinkedList[Nothing] = this

    override def foldLeft[B](base: B)(reducer: (B, Nothing) => B): B = ???

    override def apply(index: Int): Nothing = throw new NoSuchElementException("Empty")

    override def reverse: LinkedList[Nothing] = this
  }

}
