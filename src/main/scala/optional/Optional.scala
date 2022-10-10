package optional

sealed trait Optional[+A] {
  def value: A
  def isAbsent: Boolean
  def isPresent: Boolean
  def map[B](f: A => B): Optional[B]
  def flatMap[B](f: A => Optional[B]): Optional[B]
  def fold[B](ifAbsent: => B)(ifPresent: A => B): B
  def withFilter(f: A => Boolean): Optional[A]
  def foreach(f: A => Unit): Unit
  def zip[A1 >: A, B](that: Optional[B]): Optional[(A1, B)]
  def orElse[A1 >: A](other: => Optional[A1]): Optional[A1]
  def getOrElse[A1 >: A](default: => A1): A1
  def filter(f: A => Boolean): Optional[A]
  def contains[A1 >: A](elem: => A1): Boolean
  def exists(f: A => Boolean): Boolean
  def size: Int
}

object Optional {

  def apply[A](value: => A): Optional[A] = if (value == null) Absent else Present[A](value)

  def present[A](value: A): Present[A] = Present[A](value)

  def absent[A]: Optional[A] = Absent

  final case class Present[A](override val value: A) extends Optional[A] {

    override def map[B](f: A => B): Optional[B] = Optional.present(f(value))

    override def flatMap[B](f: A => Optional[B]): Optional[B] = f(value)

    override def fold[B](ifAbsent: => B)(ifPresent: A => B): B = ifPresent(value)

    override def isPresent: Boolean = true

    override def foreach(f: A => Unit): Unit = f(value)

    override def zip[A1 >: A, B](that: Optional[B]): Optional[(A1, B)] = that match {
      case Absent => Absent
      case _      => Optional.present(this.value, that.value)
    }

    override def isAbsent: Boolean = false

    override def orElse[A1 >: A](other: => Optional[A1]): Optional[A1] = this

    override def getOrElse[A1 >: A](default: => A1): A1 = value

    override def contains[A1 >: A](elem: => A1): Boolean = value == elem

    override def exists(f: A => Boolean): Boolean = f(value)

    override def size: Int = 1

    override def withFilter(f: A => Boolean): Optional[A] = if (f(value)) this else Optional.absent

    override def filter(f: A => Boolean): Optional[A] = withFilter(f)

  }

  case object Absent extends Optional[Nothing] {

    override def value: Nothing = throw new NoSuchElementException("The value is absent.")

    override def map[B](f: Nothing => B): Optional[B] = this

    override def flatMap[B](f: Nothing => Optional[B]): Optional[B] = this

    override def fold[B](ifAbsent: => B)(ifPresent: Nothing => B): B = ifAbsent

    override def isPresent: Boolean = false

    override def foreach(f: Nothing => Unit): Unit = ()

    override def zip[A1 >: Nothing, B](that: Optional[B]): Optional[(A1, B)] = this

    override def isAbsent: Boolean = true

    override def orElse[A1 >: Nothing](other: => Optional[A1]): Optional[A1] = other

    override def getOrElse[A1 >: Nothing](default: => A1): A1 = default

    override def contains[A1 >: Nothing](elem: => A1): Boolean = false

    override def exists(f: Nothing => Boolean): Boolean = false

    override def size: Int = 0

    override def withFilter(f: Nothing => Boolean): Optional[Nothing] = this

    override def filter(f: Nothing => Boolean): Optional[Nothing] = this

  }

}
