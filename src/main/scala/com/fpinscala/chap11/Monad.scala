package com.fpinscala.chap11

import com.scala.fpscala.chap7.Par
import com.scala.fpscala.chap7.Par.Par
import com.scala.fpscala.chap7.Par.Par
import com.scala.fpscala.chap8.Gen

/**
 * Created by Jegan on 8/6/2015.
 */
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  val listFunctor = new Functor[List] {
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa.map(f)
  }
}

case class Item(name: String, price: Double)
case class Order(item: Item, quantity: Int)

object Test extends App {
/*  val genOrder: Gen[Order] = for {

  }*/

  val some = Some[Int](10)
  val none = None

  def result(o: Option[Int]): Option[Int] = o.flatMap(x => Some(x + x)).flatMap(y => Some(y / 2))

//  val result = some.flatMap(x => Some(x + x)).flatMap(y => Some(y / 2))
  println(s"Result is ${result(some)}")
  println(s"Result2 is ${result(none)}")


}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def flatMapViaCompose[A, B](ma: M[A])(f: A => M[B]): M[B] = ???
  
  def flatMapViaJoin[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = lma.foldLeft(unit(List[A]()))((mla, ma) => map2(ma, mla)((a, la) => a :: la))

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] = la.foldLeft(unit(List[B]()))((mlb, a) => map2(mlb, f(a))((lb, b) => b :: lb))

//  def traverse[A, B](lma: List[M[A]])(f: A => M[B]): M[List[B]] = sequence(lma.map(ma => flatMap(ma)(f)))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    if (n <= 0) unit(List[A]())
    else map2(ma, replicateM(n - 1, ma))(_ :: _)

  def factor[A, B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((a, b) => (a, b))

  def cofactor[A, B](e: Either[M[A], M[B]]): M[Either[A, B]] = e match {
    case Left(ma) => map(ma)(Left(_))
    case Right(mb) => map(mb)(Right(_))
  }

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = a => flatMap(f(a))(g)
  
  def composeViaJoin[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = a => join(map(f(a))(g))

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)  
}

object Monad {

  val genMonad = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: (A) => Gen[B]): Gen[B] = ma flatMap f
  }

  val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }
}
