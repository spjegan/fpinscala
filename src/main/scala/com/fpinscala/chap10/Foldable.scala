package com.fpinscala.chap10

import MonoidTest._

/**
 * Created by Jegan on 8/4/2015.
 */
trait Foldable[F[_]] {

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B = foldRight(as)(m.zero)((a, b) => m.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] = foldLeft(as)(List[A]())((l, a) => a :: l)

}

object ListFoldable extends Foldable[List] {

  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid[B]): B = foldLeft(as)(m.zero)((b, a) => m.op(b, f(a)))

  override def toList[A](fa: List[A]): List[A] = fa
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(m: Monoid[B]): B = foldMap(as)(f)(m)

}

object StreamFoldable extends Foldable[Stream] {

  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(l)(foldLeft(r)(z)(f))(f)
  }

  override def foldMap[A, B](as: Tree[A])(f: A => B)(m: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => m.op(foldMap(l)(f)(m), foldMap(r)(f)(m))
  }
}

object OptionFoldable extends Foldable[Option] {

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    case Some(a) => f(a, z)
    case None => z
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
    case Some(a) => f(z, a)
    case None => z
  }

  override def foldMap[A, B](as: Option[A])(f: A => B)(m: Monoid[B]): B = as match {
    case Some(a) => f(a)
    case None => m.zero
  }
}

