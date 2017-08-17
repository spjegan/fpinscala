package com.fpinscala.chap12

import com.fpinscala.chap11.Functor

/**
 * Created by jegan on 13/9/15.
 */
trait Applicative[F[_]] extends Functor[F] {

//  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(unit(a => ))

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  def unit[A](a: A): F[A]
}
