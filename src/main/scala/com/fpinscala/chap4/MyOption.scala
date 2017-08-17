package com.fpinscala.chap4

/**
 * Created by jegan on 25/5/15.
 */
sealed trait MyOption[+A] {

  def map[B](f: A => B): MyOption[B] =  this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case None => None
    case Some(x) => f(x)
  }

  def filter(f: A => Boolean): MyOption[A] = this match {
    case Some(x) => if (f(x)) Some(x) else None
    case _ => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

//  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this getOrElse ob /*this match {
//    case None => ob
//    case _ => this
//  }*/

  def orElseByMap[B >: A](ob: => MyOption[B]): MyOption[B] = map(a => Some(a)) getOrElse ob
}

case object None extends MyOption[Nothing]

case class Some[A](value: A) extends MyOption[A]

class Opt[+A] extends MyOption[A] {



}
