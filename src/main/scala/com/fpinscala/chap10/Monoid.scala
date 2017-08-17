package com.fpinscala.chap10

/**
 * Created by Jegan on 7/31/2015.
 */
trait Monoid[A] {

  def op(a: A, b: A): A

  def zero: A
}

object MonoidTest extends App {

  val stringMonoid = new Monoid[String] {
    override def op(a: String, b: String): String = a + b

    override def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a: List[A], b: List[A]): List[A] = a ++ b

    def zero: List[A] = Nil
  }

  val intAddition = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a + b

    override def zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a * b

    override def zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    override def op(a: Boolean, b: Boolean): Boolean = a | b

    override def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    override def op(a: Boolean, b: Boolean): Boolean = a & b

    override def zero: Boolean = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    override def op(a: Option[A], b: Option[A]): Option[A] = a orElse b

    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a: A => A, b: A => A): A => A = a.andThen(b)

    override def zero: (A) => A = a => a
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a: A, b: A): A = m.op(b, a)

    override def zero: A = m.zero
  }

  def wordsMonoid: Monoid[String] = new Monoid[String] {
    override def op(a: String, b: String): String = if (a.endsWith(" ")) a.trim + " " + b.trim else a + " " + b.trim

    //    override def op(a: String, b: String): String = a.trim + " " + b.trim
    override def zero: String = ""
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  /*  def orderedIntMonoid: Monoid[Int] = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = {
      println(s"(a, b) are ($a, $b)")
      a - b
    }
    override def zero: Int = 0
}*/

  /*  def orderedIntMonoid: Monoid[(Int, Boolean)] = new Monoid[(Int, Boolean)] {
    override def op(a: (Int, Boolean), b: (Int, Boolean)): Int = {
      println(s"(a, b) are ($a, $b)")
      if (a._1 < b._1)
    }
    override def zero: Int = 0
  }*/

  def foldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty) {
      m.zero
    } else if (v.size == 1) {
      f(v.head)
    } else {
      val (l, r) = v.splitAt(v.length / 2)
      println(s"left is $l and right is $r")
      m.op(foldMap(l, m)(f), foldMap(r, m)(f))
    }
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldRightIndexedSeq[A, B](v: IndexedSeq[A])(z: B)(f: (A, B) => B): B = foldMap(v, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldLeftIndexedSeq[A, B](v: IndexedSeq[A])(z: B)(f: (B, A) => B): B = foldMap(v, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def product[A, B](m1: Monoid[A], m2: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a: (A, B), b: (A, B)): (A, B) = (m1.op(a._1, b._1), m2.op(a._2, b._2))

    override def zero: (A, B) = (m1.zero, m2.zero)
  }

  /*  def coproduct[A, B](m1: Monoid[A], m2: Monoid[B]): Monoid[Either[A, B]] = new Monoid[Either[A, B]] {
    override def op(a: Either[A, B], b: Either[A, B]): Either[A, B] = Either(m1.op(a.left.get, b.left.get), m2.op(a.right.get, b.right.get))
    override def zero: Either[A, B] = Either.
  }*/

  def mapMergeMonoid[K, V](m: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def op(a: Map[K, V], b: Map[K, V]): Map[K, V] = a.map {
      case (k, v) => (k, m.op(v, b.getOrElse(k, m.zero)))
    }

    override def zero: Map[K, V] = Map()
  }

  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] = new Monoid[(A) => B] {
    override def op(ma: (A) => B, mb: (A) => B): (A) => B = a => b.op(ma(a), mb(a))

    override def zero: (A) => B = a => b.zero
  }

  def frequencyMap(strings: IndexedSeq[String]): Map[String, Int] = {
    val m: Monoid[Map[String, Int]] = mapMergeMonoid(intAddition)
    foldMap(strings, m)(a => Map(a -> 1))

/*    def countMap(s: IndexedSeq[String], finalMap: Map[String, Int]): Map[String, Int] = {
      //    def countMap(s: IndexedSeq[String]): Map[String, Int] = {
      if (s.size == 1) {
        Map(s.head -> 1)
      } else {
        val (l, r) = s.splitAt(s.length / 2)
        println(s"left is $l")
        println(s"right is $r")
        println(s"finalMap is $finalMap")
        val lmap = countMap(l, finalMap)
        println(s"lmap - $lmap")
        val imap = m.op(lmap, finalMap)
        println(s"intermediate map - $imap")
        val rmap = countMap(r, imap)
        println(s"rmap - $rmap")
        val resultmap = m.op(lmap, rmap)
        println(s"Last map - $resultmap")
        println("\n")
        resultmap
      }
    }
    countMap(strings, Map())*/
//    strings.foldLeft(m.zero)((map, s) => m.op(map, m.op(Map(s -> 1), map)))
/*    strings.foldLeft(m.zero)((map, s) => {
      println(s"String s is $s")
      println(s"map b4 op is $map")
      val op1 = m.op(Map(s -> 1), map)
      println(s"op1 is $op1")
      val op2 = m.op(op1, map)
      println(s"op2 is $op2")
      println()
      op2
    })*/
  }

  val seq = IndexedSeq("sam", "peter", "ryan", "sam", "lola", "rohan", "jordan", "lola")
  println(s"Frequency map is ${frequencyMap(seq)}")

  val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
  val m2 = Map("o1" -> Map("i2" -> 3))

  val M: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))

  val m3 = Map("i1" -> 1, "i2" -> 2)
  val m4 = Map("i2" -> 3)

  val MM: Monoid[Map[String, Int]] = mapMergeMonoid(intAddition)

  val result = MM.op(m3, m4)
  println(s"mm map is $result")

//  val ints = Vector(1, 2, 3, 4, 5)
//  val i = foldMap(ints, orderedIntMonoid)(a => a)
//  println(s"Ordered ints is $i")

//  val vwords = Vector("John  ", " Peter", " Sam   ")
//  println(s"Words are ${foldRightIndexedSeq(vwords)(wordsMonoid.zero)(wordsMonoid.op)}...")
//  println(s"Words are ${foldLeftIndexedSeq(vwords)(wordsMonoid.zero)(wordsMonoid.op)}...")



//  val words = List("John  ", " Peter", " Sam   ")
//  println(s"Words are ${words.foldRight(wordsMonoid.zero)(wordsMonoid.op)}...")
//  println(s"Words are ${words.foldLeft(wordsMonoid.zero)(wordsMonoid.op)}...")

//  val words = List("John", "Peter", "Sam")

//  println(s"Fold Left ${words.foldLeft(stringMonoid.zero)(stringMonoid.op)}")
//  println(s"Fold Right ${words.foldRight(stringMonoid.zero)(stringMonoid.op)}")
}
