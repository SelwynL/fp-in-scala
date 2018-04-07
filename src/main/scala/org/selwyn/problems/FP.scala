package org.selwyn.problems

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object FP {

  /**
    * Exercise 2.1: Recursive function to get the nth Fibonacci number (based on zero index)
    */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)

    loop(n, 0, 1)
  }

  /**
    * Exercise 2.2: Check whether Array[A] is sorted according to the comparison
    *               function
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n == as.length) true
      else if (ordered(as(n - 1), as(n))) loop(n + 1)
      else false

    loop(1)
  }

  /**
    * Exercise 2.3: Convert function f of two args into a function with one arg
    *               that partially applies f
    */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    def c(a: A): (B => C) =
      (b: B) => f(a, b)
    c
  }

  /**
    * Exercise 2.4: Reverse the transformation of curry
    */
  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
    def u(a: A, b: B): C =
      f(a)(b)
    u
  }

  /**
    * Exercise 2.5: Compose two functions
    */
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  /**
    * Exercise 3.2: Remove the head from a list
    */
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(head, tail) => tail
    case Nil              => Nil
  }

  /**
    * Exercise 3.3: Replace first element in a list with a different value
    */
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(head, tail) => Cons(h, tail)
    case Nil              => Cons(h, Nil)
  }

  /**
    * Exercise 3.4: Drops first n element from a list
    */
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case l: Cons[A] if n == 0 => l
    case Cons(h, t)           => drop(t, n - 1)
    case Nil                  => Nil
  }

  /**
    * Exercise 3.5: Removes elements from a list prefix as long as they match a predicate
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h)  => dropWhile(t, f)
    case Cons(h, t) if !f(h) => l
    case Nil                 => Nil
  }

  /**
    * Exercise 3.6: Removes the last element of a list
    */
  def init[A](l: List[A]): List[A] = {

    // Reverse the given 'reg' list
    @annotation.tailrec
    def rev(reg: List[A], reverse: List[A]): List[A] = reg match {
      case Nil        => reverse
      case Cons(h, t) => rev(t, Cons(h, reverse))
    }

    // Loop until the last item is encountered (don't include it in the new list)
    @annotation.tailrec
    def loop(list: List[A], revrsd: List[A]): List[A] = list match {
      case Nil                 => Nil
      case Cons(h, Nil)        => revrsd
      case Cons(h, t: Cons[A]) => loop(t, Cons(h, revrsd))
    }

    rev(loop(l, Nil), Nil)
  }

  /**
    * Exercise 3.7: Can product, implemented using foldRight immediately halt the recursion and return 0.0
    *               if it encounters a 0.0
    *
    * NO
    */
  /**
    * Exercise 3.8: See what happens when you pass Nil and Cons to foldRight
    *               (This function is not stack-safe - it can result in StackOverflowError)
    */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  /**
    * Exercise 3.9: Compute the length of a list using foldRight
    */
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, n) => n + 1)

  /**
    * Exercise 3.10: Implement foldLeft
    */
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  /**
    * Exercise 3.11: Write sum, product and length using foldLeft
    */
  def sumFL(l: List[Int]) =
    foldLeft(l, 0)(_ + _)
  def productFL(l: List[Double]) =
    foldLeft(l, 1.0)(_ * _)
  def lengthFL[A](l: List[A]) =
    foldLeft(l, 0)((n, _) => n + 1)

  /**
    * Exercise 3.12: Write a function that returns the reverse of a list using a fold
    */
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((t, h) => Cons(h, t))

  /**
    * Exercise 3.13: Is it possible to write foldLeft in terms of foldRight? Yes
    *                How about the other way around? Yes
    *                Do it
    */
  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, z)((a: A, b: B) => f(b, a))
  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, z)((b: B, a: A) => f(a, b))

  /**
    * Exercise 3.14: Write append in terms of foldLeft and foldRight
    */
  def appendFR[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((item: A, list: List[A]) => Cons(item, list))
  def appendFL[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)((list: List[A], item: A) => Cons(item, list))

  /**
    * Exercise 3.15: Concatenate a list of lists into a single list
    */
  def concat[A](as: List[List[A]]): List[A] =
    //foldLeft(as, Nil: List[A])((list: List[A], l: List[A]) => appendFR(list, l))
    foldLeft(as, Nil: List[A])(appendFR)

  /**
    * Exercise 3.16: Transform a list by adding 1 to each element
    */
  def addOne(as: List[Int]): List[Int] =
    //foldLeft(reverse(as), Nil: List[Int])((l: List[Int], n: Int) => Cons(n + 1, l))
    foldRight(as, Nil: List[Int])((n: Int, l: List[Int]) => Cons(n + 1, l))

  /**
    * Exercise 3.17: Transform a list by converting each element to a string
    */
  def toString(as: List[Double]): List[String] =
    //foldLeft(reverse(as), Nil: List[String])((l: List[String], d: Double) => Cons(d.toString, l))
    foldRight(as, Nil: List[String])((d: Double, l: List[String]) => Cons(d.toString, l))

  /**
    * Exercise 3.18: Transform each element in a list according to a given function
    */
  def map[A, B](as: List[A])(f: A => B): List[B] =
    //foldLeft(reverse(as), Nil: List[B])((l: List[B], a: A) => Cons(f(a), l))
    foldRight(as, Nil: List[B])((a: A, l: List[B]) => Cons(f(a), l))

  /**
    * Exercise 3.19: Remove elements from a list unless they satisfy a given predicate
    */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a: A, l: List[A]) =>
      f(a) match {
        case true  => Cons(a, l)
        case false => l
    })
}
