package optional

import optional.Optional.{Absent, Present}

import java.util.concurrent.atomic.AtomicBoolean

class OptionalTest extends munit.FunSuite {

  test("Optional.apply(null) should return Absent") {
    assert(Optional(null) == Absent)
  }

  test("Optional.apply(5) should return Present(5)") {
    assert(Optional(5) == Present(5))
  }

  test("Optional.present(Nil) should return Present(List())") {
    assert(Optional.present(Nil) == Present(List.empty[Any]))
  }

  test("Optional.absent should return Absent") {
    assert(Optional.absent == Absent)
  }

  test("Optional(10).value should return 10") {
    assert(Optional.present(10).value == 10)
  }

  test("Absent.value should throw NoSuchElementException") {
    try {
      Optional.absent.value
    } catch {
      case nse: NoSuchElementException => assert(cond = true)
    }
  }

  test("Optional(true).isAbsent should return false") {
    assert(!Optional.present(true).isAbsent)
  }

  test("Absent.isAbsent should return true") {
    assert(Optional.absent.isAbsent)
  }

  test("Optional(true).isPresent should return true") {
    assert(Optional.present(true).isPresent)
  }

  test("Absent.isPresent should return false") {
    assert(!Optional.absent.isPresent)
  }

  test("""Optional("Hello").map(_.toUpperCase) should return Present("HELLO")""") {
    assert(Optional("Hello").map(_.toUpperCase) == Present("HELLO"))
  }

  test("""Optional("Hello").map(identity) should return Present("Hello")""") {
    assert(Optional("Hello").map(identity) == Present("Hello"))
  }

  test("""Absent.map(...) should return Absent""") {
    assert(Optional.absent[String].map(_.toLowerCase) == Absent)
  }

  test("""Absent.map(identity) should return Absent""") {
    assert(Optional.absent[String].map(identity) == Absent)
  }

  test("Optional(5).flatMap(...) should return Present(...) stringified") {
    assert(Optional.present(5).flatMap(value => Optional.present(value.toString.concat(value.toString))) == Present("55"))
  }

  test("Optional(5).flatMap(...) should return Absent") {
    assert(Optional.present(5).flatMap(value => Absent) == Absent)
  }

  test("Absent.flatMap(...) should return Absent") {
    assert(Optional.absent[Int].flatMap(_ => Optional.present(5)) == Absent)
  }

  test("""Optional(5).fold("0")(_.toString) should return "5"""") {
    assert(Optional(5).fold("0")(_.toString) == "5")
  }

  test("""Optional(null).fold("0")(_.toString) should return "0"""") {
    assert(Optional.absent[Int].fold("0")(_.toString) == "0")
  }

  test("Optional(5).withFilter(_ > 2) should return Present(5)") {
    assert(Optional.present(5).withFilter(_ > 2) == Present(5))
  }

  test("Optional(5).filter(_ < 2) should return Absent") {
    assert(Optional.present(5).filter(_ < 2) == Absent)
  }

  test("Absent.foreach(println) should do nothing") {
    val test = new AtomicBoolean()
    Optional.absent[String].foreach { str =>
      test.set(true)
    }
    assert(!test.get())
  }

  test("Optional(10).foreach(println) should exercise it") {
    val test = new AtomicBoolean()
    Optional.present[String]("10").foreach { str =>
      test.set(true)
    }
    assert(test.get())
  }

  test("Optional(10).zip(Optional(20)) should return Present((10, 20))") {
    assert(Optional.present(10).zip(Optional.present(20)) == Optional((10, 20)))
  }

  test("Optional(10).zip(Absent) should return Absent") {
    assert(Optional.present(10).zip(Optional.absent) == Absent)
  }

  test("Absent.zip(Optional(10)) should return Absent") {
    assert(Optional.absent.zip(Optional.present(10)) == Absent)
  }

  test("Absent.zip(Absent) should return Absent") {
    assert(Optional.absent.zip(Optional.absent) == Absent)
  }

  test("Optional(10).orElse(Optional(20)) should return Present(10)") {
    assert(Optional(10).orElse(Optional(20)) == Present(10))
  }

  test("Absent.orElse(Optional(20)) should return Present(20)") {
    assert(Optional.absent.orElse(Optional(20)) == Present(20))
  }

  test("Present(10).getOrElse(11) should return 10") {
    assert(Optional.present(10).getOrElse(11) == 10)
  }

  test("Absent.getOrElse(11) should return 11") {
    assert(Optional.absent.getOrElse(11) == 11)
  }

  test("Optional(10).contains(10) should return true") {
    assert(Optional.present(10).contains(10))
  }

  test("Optional(10).contains(11) should return false") {
    assert(!Optional.present(10).contains(11))
  }

  test("Absent.contains(11) should return false") {
    assert(!Optional.absent.contains(11))
  }

  test("Optional(10).exists(_ == 10) should return true") {
    assert(Optional.present(10).exists(_ == 10))
  }

  test("Optional(10).exists(_ > 10) should return false") {
    assert(!Optional.present(10).exists(_ > 10))
  }

  test("Absent[Int].exists(_ > 10) should return false") {
    assert(!Optional.absent[Int].exists(_ == 10))
  }

  test("Optional(true).size should return 1") {
    assert(Optional.present[Boolean](true).size == 1)
  }

  test("Optional.absent.size should return 0") {
    assert(Optional.absent[Int].size == 0)
  }


}
