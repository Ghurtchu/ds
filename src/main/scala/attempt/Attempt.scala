package attempt

import optional.Optional

sealed trait Attempt[+A] {
  def value: A
  def isSuccess: Boolean
  def isFailure: Boolean
  def map[B](f: A => B): Attempt[B]
  def flatMap[B](f: A => Attempt[B]): Attempt[B]
  def filter(f: A => Boolean): Attempt[A]
  def withFilter(f: A => Boolean): Attempt[A]
  def fold[B](ifSuccessful: A => B)(ifFailed: B): B
  def toOptional: Optional[A]
  def foreach(f: A => Unit): Unit
  def getOrElse[A1 >: A](default: => A1): A1
  def orElse[A1 >: A](default: => Attempt[A1]): Attempt[A1]
}

object Attempt {

  def apply[A](a: => A): Attempt[A] = {
    try Successful(a)
    catch { case t: Throwable => Failed(t) }
  }

  final case class Successful[+A](override val value: A) extends Attempt[A] {

    override def isSuccess: Boolean = true

    override def isFailure: Boolean = false

    override def map[B](f: A => B): Attempt[B] = Attempt(f(value))

    override def flatMap[B](f: A => Attempt[B]): Attempt[B] = f(value)

    override def filter(f: A => Boolean): Attempt[A] = if (f(value)) this else Failed(new NoSuchElementException("Predicate does not hold."))

    override def fold[B](ifSuccessful: A => B)(ifFailed: B): B = ifSuccessful(value)

    override def toOptional: Optional[A] = Optional.present(value)

    override def foreach(f: A => Unit): Unit = f(value)

    override def withFilter(f: A => Boolean): Attempt[A] = filter(f)

    override def getOrElse[A1 >: A](default: => A1): A1 = value

    override def orElse[A1 >: A](default: => Attempt[A1]): Attempt[A1] = this

  }

  final case class Failed[+T](throwable: Throwable) extends Attempt[T] {

    override def value: T = throw new NoSuchElementException("Failed does not hold value.")

    override def isSuccess: Boolean = false

    override def isFailure: Boolean = true

    override def map[B](f: T => B): Attempt[B] = this.asInstanceOf[Attempt[B]]

    override def flatMap[B](f: T => Attempt[B]): Attempt[B] = this.asInstanceOf[Attempt[B]]

    override def filter(f: T => Boolean): Attempt[T] = this

    override def fold[B](ifSuccessful: T => B)(ifFailed: B): B = ifFailed

    override def toOptional: Optional[T] = Optional.absent[T]

    override def foreach(f: T => Unit): Unit = ()

    override def withFilter(f: T => Boolean): Attempt[T] = this

    override def getOrElse[A1 >: T](default: => A1): A1 = default

    override def orElse[A1 >: T](default: => Attempt[A1]): Attempt[A1] = default

  }

}
