package com.fpinscala.chap5

/**
 * Created by jegan on 26/5/15.
 */
trait Stream[+A] {

  def uncons: Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons isEmpty

  def toList: List[A] = uncons match {
    case Some((h: A, t: Stream[A])) => h :: t.toList
    case None => List()
  }

  def take(n: Int): Stream[A] = uncons match {
    case Some((h: A, t: Stream[A])) if n >= 1 => Stream.cons(h, t.take(n - 1))
    case Some((h: A, _)) if n == 1 => Stream.cons(h, Stream.empty)
    case _ => Stream.empty
  }

/*  def takeViaUnfold(n: Int): Stream[A] = Stream.unfold((this, n)) {
    case (s.uncons, 1) =>  if (i >= 1) Some((s, n - 1)) else None
  }*/

  def takeWhile(p: A => Boolean): Stream[A] = uncons match {
    case Some((h: A, t: Stream[A])) if p(h) => Stream.cons(h, t takeWhile p)
    case Some((h: A, t: Stream[A])) if !p(h) => t takeWhile p
    case _ => Stream.empty
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = foldRight[Stream[A]](Stream.empty)((a, s) => if (p(a)) Stream.cons(a, s) else s)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = uncons match {
    case Some((h: A, t: Stream[A])) => t.foldRight(f(h, z))(f)
    case None => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => { println(s"A is $a"); p(a) || b } )

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => { println(s"A is $a"); p(a) && b } )

  def map[B](f: A => B): Stream[B] = foldRight[Stream[B]](Stream.empty)((a, s) => Stream.cons(f(a), s))

  def mapViaUnfold[B](f: A => B): Stream[B] = Stream.unfold(this)(s => s.uncons match {
    case Some((h: A, t: Stream[A])) => Some(f(h), t)
    case _ => None
  })

  def filter(f: A => Boolean): Stream[A] = foldRight[Stream[A]](Stream.empty)((a, s) => if (f(a)) Stream.cons(a, s) else s)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, s) => Stream.cons(a, s))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight[Stream[B]](Stream.empty)((a, s) => f(a) append s)


}

//case object Empty extends Stream[Nothing]
//case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def empty[A]: Stream[A] =
    new Stream[A] { def uncons = None }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a) { case n => Some(n, n) }

  def from[A](a: A)(f: A => A): Stream[A] = cons(a, from(f(a))(f))

  def fromViaUnfold[A](a: A)(f: A => A): Stream[A] = unfold(a) { case n => Some(n, f(n)) }

  def fibs = {
    def go(n1: Int, n2: Int): Stream[Int] = {
      val sum = n1 + n2
      cons[Int](n1, go(n2, sum))
    }
    go(0, 1)
  }

  def fibsViaUnfold = unfold((0, 1)) { case (f0, f1) => Some(f0, (f1, f0 + f1)) }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, t)) => cons(h, unfold(t)(f))
    case None => empty
  }

}