package org.selwyn.problems


/**
 * Exercise 4.1: Implement functions on Option
 */
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(v) => Some(f(v))
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    // this match {
    //     case None => None
    //     case Some(v) => f(v)
    // }
    map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case None    => default
    case Some(v) => v
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _    => this
  }

  def filter(f: A => Boolean): Option[A] =
    // this match {
    //     case Some(v) if f(v) => Some(v)
    //     case _ => None
    // }
    flatMap((v: A) => if (f(v)) Some(v) else None)

}

object FP4 {}
