package com.fpinscala.chap4

/**
  * Created by jegan on 6/7/16.
  */
object Ex4_1 {
  def apply(p: String, p2: String) = p + p2
}

object ApplyTest {
  Ex4_1("test", "test2")
}

//sealed trait MyOption[+A]
//
//case class Some[A](a: A) extends MyOption[A]
