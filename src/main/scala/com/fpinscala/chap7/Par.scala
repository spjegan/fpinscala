package com.fpinscala.chap7

import java.util.concurrent._

/**
 * Created by Jegan on 7/13/2015.
 */
/*trait Par[A] {

  def unit(a: => A): Par[A]

  def get(p: Par[A]): A

//  def map2[B](p1: (IndexedSeq[A]) => A, p2: (IndexedSeq[A] => A))(f: (A, A) => B)

  def map2[B, C](para: Par[A], parb: Par[B])(f: (A, B) => Par[C])
}*/

/*class Par[A] extends Par[A] {
  override def unit(a: => A): Par[A] = ???

  override def map2[B, C](para: Par[A], parb: Par[B])(f: (A, B) => Par[C]): Unit = ???

  override def get(p: Par[A]): A = ???
}*/

/*case class Par[A]() {
  def unit(a: => A): Par[A] = ???

  def map2[B, C](para: Par[A], parb: Par[B])(f: (A, B) => Par[C]): Unit = ???

  def get(p: Par[A]): A = ???
}*/


object Par extends App {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(p: Par[A]): Future[A] = {
    println(s"Inside run $p")
    p(s)
  }

  def unit[A](a: A): Par[A] = e => {
      println(s"Inside unit $a")
      e.submit(new Callable[A]() {
      override def call(): A = a
    })
  }

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    e => {
      println(s"Inside map2 $pa, $pb")
      val fa = pa(e)
      val fb = pb(e)
      val a = fa.get()
      val b = fb.get()
      println(s"A is $a")
      println(s"B is $b")
      unit(f(a, b))(e)
    }

/*  def map2UsingProductAndMap[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = e => {
    val pab = product(pa, pb)
//    val (a, b) = pab(e)

    map(pab)( (a: A, b: B) =>
  }*/

  def fork[A](p: => Par[A]): Par[A] = e => {
    println(s"Inside fork $p")
    e.submit(new Callable[A]() {
      override def call(): A = run(e)(p).get()
    })
  }

  def async[A](a: A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => async(f(a))

  def sortPar(p: Par[List[Int]]): Par[List[Int]] = map2(p, unit(()))((a, _) => a.sorted)

  def sortParUsingMap(p: Par[List[Int]]): Par[List[Int]] = map(p)(l => l.sorted)

  def map[A, B](p: Par[A])(f: A => B): Par[B] = e => unit(f(p(e).get()))(e)

  def mapUsingMap2[A, B](p: Par[A])(f: A => B): Par[B] = map2(p, unit(()))((a, _) => f(a))

  def product[A, B](pa: Par[A], pb: Par[B]): Par[(A, B)] = e => {
    val fa = pa(e)
    val fb = pb(e)
    unit((fa.get(), fb.get()))(e)
  }

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = fork(sequence(l.map(asyncF(f))))

  def sequence[A](l: List[Par[A]]): Par[List[A]] =  l.foldRight[Par[List[A]]](unit(List()))((p, l) => map2(p, l)(_ :: _))


  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = l map asyncF((a: A) => if (f(a)) List(a) else List())
    map(sequence(pars))(_.flatten)
  }

  def choice[A](pa: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = e => {
    val a = run(e)(pa).get()
    if (a) run(e)(ifTrue) else run(e)(ifFalse)
  }

  def choiceViaChoiceN[A](pa: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = e => {
    val a = run(e)(pa).get()
    val l = List(ifTrue, ifFalse)
    if (a) choiceN(unit(0))(l)(e) else choiceN(unit(1))(l)(e)
  }

  def choiceViaMapAndChoiceN[A](pa: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = choiceN(map(pa)(t => if(t) 1 else 0))(List(ifTrue, ifFalse))

  def choiceN[A](pa: Par[Int])(choices: List[Par[A]]): Par[A] = e => {
    val i = run(e)(pa).get()
    run(e)(choices(i))
  }

  def choiceMap[A, B](pa: Par[A])(choices: Map[A, Par[B]]): Par[B] = e => {
    val a = run(e)(pa).get()
    run(e)(choices(a))
  }

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = e => {
    val a = run(e)(pa).get()
    run(e)(f(a))
  }

  def flatMapViaJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = e => {
    val j = run(e)(join(unit(pa))).get()
    run(e)(f(j))
  }

  def choiceViaChooser[A](pa: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = flatMap(pa)(t => if (t) ifTrue else ifFalse)

  def choiceNViaChooser[A](pa: Par[Int])(choices: List[Par[A]]): Par[A] = flatMap(pa)(a => choices(a))

  def join[A](ppa: Par[Par[A]]): Par[A] = e => {
    val pa = run(e)(ppa).get()
    run(e)(pa)
  }

  def joinViaFlatMap[A](ppa: Par[Par[A]]): Par[A] = flatMap(ppa)(pa => pa)

  def sum(s: IndexedSeq[Int]): Par[Int] = {
    println(s"Inside sum $s")
    if (s.length <= 1) unit(s.headOption getOrElse 0)
    else {
      val (l, r) = s.splitAt(s.length / 2)
      map2(fork(sum(l)), fork(sum(r)))(_ + _)
    }
  }

  val c: Par[Int] = sum(IndexedSeq(1, 2, 3, 4, 5))
  println(s"Sum called $c")

  val e = Executors.newFixedThreadPool(10)
  println("Running sum")
  val sum = run(e)(c)
  println("Run sum")

  println(s"Sum is ${sum.get()}")

  e.shutdown()
  e.awaitTermination(Long.MaxValue, TimeUnit.MILLISECONDS)


/*  def sum(s: IndexedSeq[Int]): Par[Int] = {
    if (s.length <= 1) Par.unit(s.headOption getOrElse 0)
    else {
      val (l, r) = s.splitAt(s.length / 2)
      val parl = new Parallel().unit(sum(l))
      val parr = new Parallel().unit(sum(r))
//      sum(l) + sum(r)
//      parl.get(parl) + parl.get(parr)
      map2(sum(l), sum(r))(_ + _)
    }
  }

  println(s"Sum is ${sum(IndexedSeq(1, 2, 3, 4, 5))}")*/
}
