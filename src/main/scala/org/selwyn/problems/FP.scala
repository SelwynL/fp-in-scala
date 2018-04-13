package org.selwyn.problems

sealed trait FPList[+A] // `FPList` data type, parameterized on a type, `A`
case object Nil extends FPList[Nothing] // A `FPList` data constructor representing the empty list
case class Cons[+A](head: A, tail: FPList[A]) extends FPList[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `FPList[A]`, which may be `Nil` or another `Cons`.

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
  def tail[A](l: FPList[A]): FPList[A] = l match {
    case Cons(head, tail) => tail
    case Nil              => Nil
  }

  /**
    * Exercise 3.3: Replace first element in a list with a different value
    */
  def setHead[A](l: FPList[A], h: A): FPList[A] = l match {
    case Cons(head, tail) => Cons(h, tail)
    case Nil              => Cons(h, Nil)
  }

  /**
    * Exercise 3.4: Drops first n element from a list
    */
  def drop[A](l: FPList[A], n: Int): FPList[A] = l match {
    case l: Cons[A] if n == 0 => l
    case Cons(h, t)           => drop(t, n - 1)
    case Nil                  => Nil
  }

  /**
    * Exercise 3.5: Removes elements from a list prefix as long as they match a predicate
    */
  def dropWhile[A](l: FPList[A], f: A => Boolean): FPList[A] = l match {
    case Cons(h, t) if f(h)  => dropWhile(t, f)
    case Cons(h, t) if !f(h) => l
    case Nil                 => Nil
  }

  /**
    * Exercise 3.6: Removes the last element of a list
    */
  def init[A](l: FPList[A]): FPList[A] = {

    // Reverse the given 'reg' list
    @annotation.tailrec
    def rev(reg: FPList[A], reverse: FPList[A]): FPList[A] = reg match {
      case Nil        => reverse
      case Cons(h, t) => rev(t, Cons(h, reverse))
    }

    // Loop until the last item is encountered (don't include it in the new list)
    @annotation.tailrec
    def loop(list: FPList[A], revrsd: FPList[A]): FPList[A] = list match {
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
  def foldRight[A, B](as: FPList[A], z: B)(f: (A, B) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  /**
    * Exercise 3.9: Compute the length of a list using foldRight
    */
  def length[A](as: FPList[A]): Int =
    foldRight(as, 0)((_, n) => n + 1)

  /**
    * Exercise 3.10: Implement foldLeft
    */
  def foldLeft[A, B](as: FPList[A], z: B)(f: (B, A) => B): B = as match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  /**
    * Exercise 3.11: Write sum, product and length using foldLeft
    */
  def sumFL(l: FPList[Int]) =
    foldLeft(l, 0)(_ + _)
  def productFL(l: FPList[Double]) =
    foldLeft(l, 1.0)(_ * _)
  def lengthFL[A](l: FPList[A]) =
    foldLeft(l, 0)((n, _) => n + 1)

  /**
    * Exercise 3.12: Write a function that returns the reverse of a list using a fold
    */
  def reverse[A](l: FPList[A]): FPList[A] =
    foldLeft(l, Nil: FPList[A])((t, h) => Cons(h, t))

  /**
    * Exercise 3.13: Is it possible to write foldLeft in terms of foldRight? Yes
    *                How about the other way around? Yes
    *                Do it
    */
  def foldLeftViaFoldRight[A, B](as: FPList[A], z: B)(f: (B, A) => B): B =
    foldRight(as, z)((a: A, b: B) => f(b, a))
  def foldRightViaFoldLeft[A, B](as: FPList[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, z)((b: B, a: A) => f(a, b))

  /**
    * Exercise 3.14: Write append in terms of foldLeft and foldRight
    */
  def appendFR[A](a1: FPList[A], a2: FPList[A]): FPList[A] =
    foldRight(a1, a2)((item: A, list: FPList[A]) => Cons(item, list))
  def appendFL[A](a1: FPList[A], a2: FPList[A]): FPList[A] =
    foldLeft(reverse(a1), a2)((list: FPList[A], item: A) => Cons(item, list))

  /**
    * Exercise 3.15: Concatenate a list of lists into a single list
    */
  def concat[A](as: FPList[FPList[A]]): FPList[A] =
    //foldLeft(as, Nil: FPList[A])((list: FPList[A], l: FPList[A]) => appendFR(list, l))
    foldLeft(as, Nil: FPList[A])(appendFR)

  /**
    * Exercise 3.16: Transform a list by adding 1 to each element
    */
  def addOne(as: FPList[Int]): FPList[Int] =
    //foldLeft(reverse(as), Nil: FPList[Int])((l: FPList[Int], n: Int) => Cons(n + 1, l))
    foldRight(as, Nil: FPList[Int])((n: Int, l: FPList[Int]) => Cons(n + 1, l))

  /**
    * Exercise 3.17: Transform a list by converting each element to a string
    */
  def toString(as: FPList[Double]): FPList[String] =
    //foldLeft(reverse(as), Nil: FPList[String])((l: FPList[String], d: Double) => Cons(d.toString, l))
    foldRight(as, Nil: FPList[String])((d: Double, l: FPList[String]) => Cons(d.toString, l))

  /**
    * Exercise 3.18: Transform each element in a list according to a given function
    */
  def map[A, B](as: FPList[A])(f: A => B): FPList[B] =
    //foldLeft(reverse(as), Nil: FPList[B])((l: FPList[B], a: A) => Cons(f(a), l))
    foldRight(as, Nil: FPList[B])((a: A, l: FPList[B]) => Cons(f(a), l))

  /**
    * Exercise 3.19: Remove elements from a list unless they satisfy a given predicate
    */
  def filter[A](as: FPList[A])(f: A => Boolean): FPList[A] =
    foldRight(as, Nil: FPList[A])((a: A, l: FPList[A]) =>
      f(a) match {
        case true  => Cons(a, l)
        case false => l
    })

  /**
    * Exercise 3.20: write flatMap where the mapping function returns a list which
    * should be inserted into the final list
    */
  def flatMap[A, B](as: FPList[A])(f: A => FPList[B]): FPList[B] = {
    concat(map(as)(f))
  }

  /**
    * Exercise 3.21: use flatMap to implement filter
    */
  def filterFM[A](as: FPList[A])(f: A => Boolean): FPList[A] =
    flatMap(as)((a: A) =>
      f(a) match {
        case true  => Cons(a, Nil)
        case false => Nil
    })

  /**
    * Exercise 3.22: write a function that accepts two lists and constructs
    * a new list by adding corresponding elements
    */
  def add(as1: FPList[Int], as2: FPList[Int]): FPList[Int] = {
    def loop(a1: FPList[Int], a2: FPList[Int], combined: FPList[Int]): FPList[Int] = (a1, a2) match {
      case (Cons(h1, t1: FPList[Int]), Cons(h2, t2: FPList[Int])) =>
        loop(t1, t2, appendFR(combined, Cons(h1 + h2, Nil)))
      case (Nil, Nil)           => combined
      case (a1: Cons[Int], Nil) => appendFR(combined, a1) // append leftovers from a1 when it is longer than a2
      case (Nil, a2: Cons[Int]) => appendFR(combined, a2) // append leftovers from a2 when it is longer than a1
    }

    loop(as1, as2, Nil)
  }

  /**
    * Exercise 3.23: generalize the function from 3.22 as not specific to Int's nor the addition operation
    */
  def zipWith[A, B, C](as1: FPList[A], as2: FPList[B])(f: (A, B) => C): FPList[C] = (as1, as2) match {
    case (_, Nil)                     => Nil
    case (Nil, _)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

}
