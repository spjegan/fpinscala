package com.fpinscala.chap6

/**
 * Created by Jegan on 7/10/2015.
 */
case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, s1) = run(s)
    (f(a), s1)
  })

  def mapViaFlatMap[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {
    val (a, s1) = run(s)
    val (b, s2) = sb.run(s1)
    (f(a, b), s2)
  })

  def map2ViaFlatMap[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](s: List[State[S, A]]): State[S, List[A]] = s.foldRight(unit[S, List[A]](List()))((s1: State[S, A], l) => s1.map2(l)(_ :: _))
}
