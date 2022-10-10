package attempt

import optional.Optional

import scala.util.Try

sealed trait Attempt[+A] {
  def value: A
  def isSuccess: Boolean
  def isFailure: Boolean
  def map[B](f: A => B): Attempt[B]
  def flatMap[B](f: A => Attempt[B]): Attempt[B]
  def filter(f: A => Boolean): Attempt[A]
  def fold[B](ifSuccessful: A => B)(ifFailed: B): B
  def toOptional: Optional[A]
  def foreach(f: A => Unit): Unit
}

object Attempt {

  final case class Successful[+A](override val value: A) extends Attempt[A] {

    override def isSuccess: Boolean = ???

    override def isFailure: Boolean = ???

    override def map[B](f: A => B): Attempt[B] = ???

    override def flatMap[B](f: A => Attempt[B]): Attempt[B] = ???

    override def filter(f: A => Boolean): Attempt[A] = ???

    override def fold[B](ifSuccessful: A => B)(ifFailed: B): B = ???

    override def toOptional: Optional[A] = ???

    override def foreach(f: A => Unit): Unit = ???

  }

  final case class Failed[+T](throwable: Throwable) extends Attempt[T] {

    override def value: T = ???

    override def isSuccess: Boolean = ???

    override def isFailure: Boolean = ???

    override def map[B](f: T => B): Attempt[B] = ???

    override def flatMap[B](f: T => Attempt[B]): Attempt[B] = ???

    override def filter(f: T => Boolean): Attempt[T] = ???

    override def fold[B](ifSuccessful: T => B)(ifFailed: B): B = ???

    override def toOptional: Optional[T] = ???

    override def foreach(f: T => Unit): Unit = ???

  }

}
