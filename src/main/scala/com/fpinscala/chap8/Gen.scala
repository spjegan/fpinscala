package com.fpinscala.chap8

import com.fpinscala.chap6.{RNG, State}

/**
 * Created by Jegan on 7/20/2015.
 */
case class Gen[+A](sample: State[RNG, A], exhaustive: Stream[Option[A]]) {

/*  def map[B](f: A => B): Gen[B] = {
    val gb = sample.map(f)
    val (i, _) = gb.run(RNG.simple(123))
    Gen(gb, Stream(Some(i)))
  }*/

  def map[B](f: A => B): Gen[B] = flatMap(a => Gen.unit(f(a)))

  def flatMap[B](f: A => Gen[B]): Gen[B] = f(sample.run(RNG.simple(1234))._1)

/*  def map[B](f: A => B): Gen[B] = {
    val sb = sample.map(f)
    Gen(sb, Stream(Some(sb.map(RNG.positiveInt))))
  }*/

}

object Gen {

  def unit[A](a: A): Gen[A] = Gen(State.unit(a), Stream(Some(a)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val s = State(RNG.positiveInt).map(n => start + n % (stopExclusive - start))
    val (i, _) = s.run(RNG.simple(123))
    Gen(s, Stream(Some(i)))
  }
}
