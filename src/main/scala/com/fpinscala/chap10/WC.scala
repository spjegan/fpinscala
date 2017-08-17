package com.fpinscala.chap10

/**
 * Created by Jegan on 8/3/2015.
 */
sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object WCTest extends App {

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a: WC, b: WC): WC = (a, b) match {
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
      case (Stub(s1), Part(ls, n, rs)) => Part(s1 + ls, n, rs)
      case (Part(ls, n, rs), Stub(s1)) => Part(ls, n, rs + s1)
      case (Part(ls1, n1, rs1), Part(ls2, n2, rs2)) => Part(ls1, n1 + (if ((rs1 + ls2).isEmpty) 0 else 1) + n2, rs2)
    }
    override def zero: WC = Stub("")
  }

  def wordCount(str: String): Int = {
    def wc(c: Char): WC = {
      println(s"Character is $c")
      if (c.isWhitespace) Part("", 0, "")
      else Stub(c.toString)
    }

    def unstub(s: String) = s.length min 1

    MonoidTest.foldMap(str.toList, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(ls, n, rs) => unstub(ls) + n + unstub(rs)
    }
  }

  println(s"Count of words is ${wordCount("lorem ipsum dolor sit amet, sdfsdf")}")
}
