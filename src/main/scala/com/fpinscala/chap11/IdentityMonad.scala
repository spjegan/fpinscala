package com.fpinscala.chap11

import com.fpinscala.chap6.State

/**
 * Created by Jegan on 8/14/2015.
 */
case class Id[A](value: A) {

  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)

}

object IdentityMonad extends App {

  type IntState[A] = State[Int, A]

  val intStateMonad = new Monad[IntState] {
    override def unit[A](a: => A): IntState[A] = State(s => (a, s))

    override def flatMap[A, B](ma: IntState[A])(f: (A) => IntState[B]): IntState[B] = ma flatMap f
  }

  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](id: Id[A])(f: (A) => Id[B]): Id[B] = id flatMap f
  }

  def stateMonad[S] = new Monad[( { type lambda[x] = State[S, x] }) #lambda] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](sa: State[S, A])(f: (A) => State[S, B]): State[S, B] = sa flatMap f
  }

  val f = for {
    a <- Id("Hello")
    b <- Id(" World")
  } yield a + b

  println(s"For comprehension $f")

  val f1 = Id("Hello") flatMap (a => Id(" World") map (b => a + b))
  println(s"For comprehension $f1")

}
