package com.fpinscala.chap9

import scala.language.higherKinds

/**
 * Created by Jegan on 7/30/2015.
 */
trait Parsers[ParserError, Parser[+_]] { self =>

  def char(c: Char): Parser[Char]

  implicit def string(s: String): Parser[String]



  def *(s: String)(f: Char => Boolean): Parser[Int]

  def +(s: String)(f: Char => Boolean): Either[ParserError, Parser[Int]]

  def count3[A](s: String): Parser[(Int, Int)]



  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  def orString(s1: String, s2: String): Parser[String]

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def run[A](parser: Parser[A])(input: String): Either[ParserError, A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

/*  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: List[String]): Boolean = in.foreach(s => run(p1)(s) == run(p2)(s))

  }*/

  case class ParserOps[A](p: Parser[A]) {

    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def many(p: Parser[A]): Parser[List[A]] = ???

    def map[B](p: Parser[A])(f: A => B): Parser[B] = ???



    def *[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
  }

}
