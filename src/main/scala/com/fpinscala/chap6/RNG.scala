package com.fpinscala.chap6

/**
 * Created by Jegan on 7/2/2015.
 */
trait RNG {

  def nextInt: (Int, RNG)
}

object RNG {

  def simple(seed: Long): RNG = new RNG {
    override def nextInt = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }

  def nextInt(rng: RNG): Rand[Int] = {
    val (i, _) = rng.nextInt
    unit(i)
  }

  def nextDouble(rng: RNG): (Double, RNG) = {
    val (p, r) = RNG.positiveInt(rng)
    val d = p / (Int.MaxValue.toDouble + 1)
//    println (s"p is $p")
//    println (s"d is $d")
    (d, r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = RNG.nextDouble(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = RNG.intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = RNG.nextDouble(rng)
    val (d2, r2) = RNG.nextDouble(r1)
    val (d3, r3) = RNG.nextDouble(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG) : (List[Int], RNG) = {

/*    if (count <= 0) {
      (List(), rng)
    }
    else {
      val (i, r) = rng.nextInt
      val (j, r2) = ints(count - 1)(r)
      (i :: j, r2)
    }*/

    def ints(count: Int, rng: RNG, list: List[Int]): (List[Int], RNG) = {
      if (count <= 0) {
        (list, rng)
      } else {
        val (i, r) = rng.nextInt
        ints(count - 1, r, i :: list)
      }
    }
    ints(count, rng, List())
  }

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (r1, r) = rng.nextInt
    val (r2, newRng) = r.nextInt
    ((r1, r2), newRng)
  }

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (i, newRng) = rng.nextInt
    if (i == Int.MinValue) (i - 1, newRng)
    else (i.abs, newRng)
  }

/*  def positiveIntFlatMap(rng: RNG): Rand[Int] = {
/*    flatMap(unit(rng.nextInt))(a => r => {
      if (a == Int.MinValue) (a - 1, r)
      else (a.abs, r)
    })*/
//    flatMap(unit(rng.nextInt))(a => r => {
    flatMap(unit(RNG.nextInt))(a => {
      if (a == Int.MinValue) (a - 1, r)
      else (a.abs, r)
    })
  }*/

  def positiveIntLessThan(n: Int): Rand[Int] = {
    flatMap(positiveInt)(a => unit(a % n))
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](rand: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, r) = rand(rng)
      (f(a), r)
    }
  }

  def mapUsingFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = flatMap(r)(a => unit(f(a)))

  def map2UsingFlatMap[A, B, C](r1: Rand[A], r2: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(r1)(a => map(r2)(b => f(a, b)))

  def map2[A, B, C](rand1: Rand[A], rand2: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng1 => {
      val (a, r1) = rand1(rng1)
      val (b, r2) = rand2(r1)
      (f(a, b), r2)
    }
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val a = f(rng)
      g(a._1)(a._2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))((l, r) => map2(l, r)((a, b) => a :: b))

  def intDoubleMap(): Rand[(Int, Double)] = map2(positiveInt, nextDouble)((a, b) => (a, b))

  def doubleIntMap(): Rand[(Double, Int)] = map2(nextDouble, positiveInt)((a, b) => (a, b))

  def positiveMax(n: Int): Rand[Int] = map(positiveInt)(_ % n)

  def nextDoubleMap(): Rand[Double] = map(positiveInt)(_ / (Int.MaxValue.toDouble + 1))

}

object RNGTest extends App {
//  println(RNG.randomPair(RNG.simple(1)))
//  println(s"Min Value ${Int.MinValue}")
//  println(s"Min Value -1 ${Int.MinValue - 1}")
//  println(s"Max Value ${Int.MaxValue}")
//  println(s"Abs of Min Value ${Int.MinValue.abs}")
//  println(RNG.positiveInt(RNG.simple(1)))

//  println(new Random().nextDouble())
/*  println(s"Max Value + 1 ${Int.MaxValue + 1}")
  println(RNG.nextDouble(RNG.simple(123)))
  println(RNG.intDouble(RNG.simple(123)))
  println(RNG.doubleInt(RNG.simple(123)))
  println(RNG.double3(RNG.simple(123)))*/
//  println(RNG.ints(5)(RNG.simple(12345)))

//  val rand = RNG.positiveMax(100)
//  val (i, r) = RNG.positiveMax(500)(RNG.simple(1235))
//  val (i, r) = RNG.nextDoubleMap()(RNG.simple(1235))
//  val (i, r) = rand(RNG.simple(1235))
//  println(i)

//  val ((i, d), _) = RNG.intDoubleMap()(RNG.simple(12345))
//  println(s"Int is $i and Double is $d")

  val l = List(RNG.unit(1), RNG.unit(2), RNG.unit(3), RNG.unit(4))
  val (j, r) = RNG.sequence(l)(RNG.simple(12345))

  println(s"J is $j")


}