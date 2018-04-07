package org.selwyn.problems

import org.scalatest._

class FPSpec extends FunSuite {

  test("2.1 fib should get the nth fibonacci number") {
    assert(FP.fib(0) === 0)
    assert(FP.fib(1) === 1)
    assert(FP.fib(2) === 1)
    assert(FP.fib(3) === 2)
    assert(FP.fib(4) === 3)
    assert(FP.fib(5) === 5)
  }

  test("2.2 isSorted should detect sorted array") {
    assert(FP.isSorted(Array(1, 2, 3, 5), (a: Int, b: Int) => a <= b) === true)
    assert(FP.isSorted(Array(1, 1, 3, 4), (a: Int, b: Int) => a <= b) === true)
  }

  test("2.2 isSorted should detect unsorted array") {
    assert(FP.isSorted(Array(2, 1, 3, 4), (a: Int, b: Int) => a <= b) === false)
    assert(FP.isSorted(Array(1, 2, 3, 1), (a: Int, b: Int) => a <= b) === false)
  }

  test("2.3 curry should curry a function") {}

  test("2.4 uncurry should reverse the transformation of curry") {}

  test("2.5 compose should combine the out into the in of another function") {
    val foo: (Int => Array[String]) =
      (n: Int) => Array.fill[String](n)("Foo")

    val bar: (Array[String] => String) =
      (a: Array[String]) => a.mkString("bar ")

    assert(FP.compose(bar, foo)(2) === "Foobar Foo")
  }

  test("3.2 tail should remove first element in the list") {
    assert(FP.tail(Cons(1, Cons(2, Cons(3, Nil)))) === Cons(2, Cons(3, Nil)))
    assert(FP.tail(Cons(1, Nil)) === Nil)
    assert(FP.tail(Nil) === Nil)
  }

  test("3.3 setHead should replace the first element in the list") {
    assert(FP.setHead(Cons(1, Cons(2, Cons(3, Nil))), 5) === Cons(5, Cons(2, Cons(3, Nil))))
    assert(FP.setHead(Nil, 5) === Cons(5, Nil))
  }

  test("3.4 drop should remove the first n elements from a list") {
    assert(FP.drop(Cons(1, Cons(2, Cons(3, Nil))), 0) === Cons(1, Cons(2, Cons(3, Nil))))
    assert(FP.drop(Cons(1, Cons(2, Cons(3, Nil))), 2) === Cons(3, Nil))
    assert(FP.drop(Cons(1, Cons(2, Cons(3, Nil))), 3) === Nil)
    assert(FP.drop(Cons(1, Cons(2, Cons(3, Nil))), 5) === Nil)
    assert(FP.drop(Nil, 5) === Nil)
  }

  test("3.5 dropWhile should remove elements from the list prefix as long as they match the predicate") {
    val odd: Int => Boolean = (n) => n % 2 != 0

    assert(FP.dropWhile(Cons(1, Cons(2, Cons(3, Nil))), odd) === Cons(2, Cons(3, Nil)))
    assert(FP.dropWhile(Cons(2, Cons(14, Cons(100, Nil))), odd) === Cons(2, Cons(14, Cons(100, Nil))))
    assert(FP.dropWhile(Cons(1, Cons(11, Cons(3, Nil))), odd) === Nil)
    assert(FP.dropWhile(Nil, odd) === Nil)
  }

  test("3.6 init should drop the last element from the list") {
    assert(FP.init(Cons(1, Cons(2, Cons(3, Nil)))) === Cons(1, Cons(2, Nil)))
    assert(FP.init(Nil) === Nil)
  }

  test("3.7 ... erm NO") {}

  test("3.8 foldRight should work when passed Nil and Cons") {
    val list: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))
    assert(FP.foldRight(list, Nil: List[Int])(Cons(_, _)) === Cons(1, Cons(2, Cons(3, Nil))))
  }

  test("3.9 length should compute length of a list") {
    assert(FP.length(Cons(1, Cons(2, Cons(3, Nil)))) === 3)
    assert(FP.length(Nil) === 0)
  }

  test("3.10 foldLeft should ...") {}

  test("3.11 foldLeft should compute sum, product, and length") {
    assert(FP.sumFL(Cons(1, Cons(2, Cons(4, Nil)))) === 7)
    assert(FP.productFL(Cons(1, Cons(2, Cons(4, Nil)))) === 8)
    assert(FP.lengthFL(Cons(1, Cons(2, Cons(4, Nil)))) === 3)

    assert(FP.sumFL(Nil) === 0)
    assert(FP.productFL(Nil) === 1.0) //TODO: shouldn't this equal 0.0??
    assert(FP.lengthFL(Nil) === 0)
  }

  test("3.12 reverse should reverse a list") {
    assert(FP.reverse(Cons(1, Cons(2, Cons(3, Nil)))) === Cons(3, Cons(2, Cons(1, Nil))))
  }

  test("3.13 foldLeftViaFoldRight and foldRightViaFoldLeft should ...") {}

  test("3.14 append using fold should add one list to another") {
    val a1: List[Int] = Cons(1, Cons(2, Nil))
    val a2: List[Int] = Cons(3, Nil)
    val expected: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))

    assert(FP.appendFR(a1, a2) === expected)
    assert(FP.appendFL(a1, a2) === expected)
  }

  test("3.15 concatenate should combine a list of lists into a single list") {
    val l: List[List[Int]] =
      Cons(Cons(1, Cons(1, Cons(1, Nil))),
           Cons(Cons(2, Cons(2, Cons(2, Nil))), Cons(Cons(3, Cons(3, Cons(3, Nil))), Nil)))

    val expected: List[Int] =
      Cons(1, Cons(1, Cons(1, Cons(2, Cons(2, Cons(2, Cons(3, Cons(3, Cons(3, Nil)))))))))

    assert(FP.concat(l) === expected)
  }

  test("3.16 addOne should transform a list of integers by adding one to each element") {
    assert(FP.addOne(Cons(3, Cons(2, Cons(1, Nil)))) === Cons(4, Cons(3, Cons(2, Nil))))
  }

  test("3.17 toString should transform a list of doubles to a list of strings") {
    assert(FP.toString(Cons(3.3, Cons(2.2, Cons(1.1, Nil)))) === Cons("3.3", Cons("2.2", Cons("1.1", Nil))))
  }

  test("3.18 map should transform a list based on a given transformation function") {
    val toString: (Double => String) = _.toString
    assert(FP.map(Cons(3.3, Cons(2.2, Cons(1.1, Nil))))(toString) === Cons("3.3", Cons("2.2", Cons("1.1", Nil))))
  }

  test("3.19 filter should remove elements that do not statisfy the predicate") {
    val even: (Int => Boolean) = _ % 2 == 0
    assert(FP.filter(Cons(1, Cons(2, Cons(4, Nil))))(even) === Cons(2, Cons(4, Nil)))
  }
}
