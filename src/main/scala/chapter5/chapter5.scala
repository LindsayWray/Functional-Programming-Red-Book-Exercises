package chapter5

import chapter5.Stream.unfold

sealed trait Stream[+A] {
  def toList: List[A] = {
    def go(str: Stream[A], lst: List[A]): List[A] = {
      str match
        case Empty => lst
        case Cons(h, t) => go(t(), lst :+ h())
    }
    go(this, Nil)
  }

  def take(n: Int): Stream[A] = {
    def go(src: Stream[A], dst: Stream[A], n: Int): Stream[A] = {
      if (n == 0) dst
      else {
        src match
          case Empty => dst
          case Cons(h, t) => Stream.cons(h(), go(t(), dst, n - 1))
      }
    }
    go(this, Empty, n)
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n)) {
        case (_, 0) => None
        case (Empty, _) => None
        case (Cons(h, t), n) => Some(h(), (t(), n - 1))
    }
  }

  def drop(n: Int): Stream[A] = {
    def go(str: Stream[A], acc: Int): Stream[A] = {
      str match
        case Empty => str
        case _ if (acc >= n) => str
        case Cons(_, t) => go(t(), acc + 1)
    }
    go(this, 0)
  }

  // all elem that meet the criteria
  def takeWhile(p: A => Boolean): Stream[A] = {
    def go(src: Stream[A], dst: Stream[A]): Stream[A] = {
      src match
        case Empty => dst
        case Cons(h, t) if p(h()) => Stream.cons(h(), go(t(), dst))
        case Cons(_, t) => go(t(), dst)
    }
    go(this, Empty)
  }

  // until the first elem that does not meet criteria
  def takeWhile2(p: A => Boolean): Stream[A] = {
    def go(src: Stream[A], dst: Stream[A]): Stream[A] = {
      src match
        case Cons(h, t) if p(h()) => Stream.cons(h(), go(t(), dst))
        case _ => dst
    }
    go(this, Empty)
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    this match
      case Empty => true
      case Cons(h, t) => p(h()) && t().forAll(p)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight[Stream[A]](Empty)((a, b) => if p(a) then Stream.cons(a, b) else Empty)
  }

  def headOptionViaFoldRight: Option[A] = {
    foldRight[Option[A]](None)((h, _) => Some(h))
  }

  def mapViaFoldRight[B](f: A => B): Stream[B]= {
    foldRight[Stream[B]](Empty)((a, b) => Stream.cons(f(a), b))
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    }
  }

  def filterViaFoldRight(f: A => Boolean): Stream[A] = {
    foldRight[Stream[A]](Empty)((a, b) => if f(a) then Stream.cons(a, b) else b)
  }

  def appendViaFoldRight[B >: A](r: => Stream[B]): Stream[B] = {
    foldRight(r)((a, b) => Stream.cons(a, b))
  }

  def zipWithViaUnfold[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, s)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    }
  }

  def zipAll[B](s: Stream[B]): Stream[(Option[A],Option[B])] = {
    unfold((this, s)) {
      case (Empty, Empty) => None
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    }
  }

  def startsWith[A](s: Stream[A]): Boolean = {
    zipAll(s).takeWhileViaUnfold(e => e._2 != None).mapViaUnfold(e => e._1 == e._2).forAll(_ == true)
  }

  def tails: Stream[Stream[A]] = {
    unfold(this) (e => {
      e match
        case Empty => None
        case Cons(_, t) => Some(e, t())
    })
  }

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    foldRight((z, Stream(z)))((a, b) => {
      val value = f(a, b._1)
      (value, Stream.cons(value, b._2))
    })._2
  }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  lazy val ones: Stream[Int] = Stream.cons(1, ones)

  def onesViaUnfold: Stream[Int] = {
    unfold(1)(e => Some(e, e))
  }

  def constant[A](a: A): Stream[A] = {
    lazy val constants: Stream[A] = Stream.cons(a, constants)
    constants
  }

  def constantViaUnfold[A](a: A): Stream[A] = {
    unfold(a)(e => Some(e, e))
  }

  def from(n: Int): Stream[Int] = {
    lazy val ints: Stream[Int] = Stream.cons(n, from(n + 1))
    ints
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(e => Some(e, e + 1))
  }

  def fibs: Stream[Int] = {
    def go(prev: Int, next: Int): Stream[Int] = {
      Stream.cons(prev, go(next, prev + next))
    }
    go(0, 1)
  }

  def fibsViaUnfold: Stream[Int] = {
    unfold((0,1))((prev, next) => Some(prev, (next, prev + next)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match
      case None => Empty
      case Some(value,state) => Stream.cons(value, unfold(state)(f))
  }
}

@main def main() : Unit = {
  // exercise 5.1
  //val stream : Stream[Int] = Stream.cons(3, Stream.cons(5, Stream.cons(6, Stream.cons(1,Empty))))
  val stream : Stream[Int] = Stream(3,5,6,1)

  println("*** tolist function ***")
  println(stream.toList)

  // exercise 5.2
  println("*** take function ***")
  println(stream.take(2).toList)
  println(stream.take(6).toList) // test input longer than list size

  println("*** drop function ***")
  println(stream.drop(2).toList)
  println(stream.drop(5).toList) // returns empty list

  // exercise 5.3
  println("*** takeWhile function ***")
  println(stream.takeWhile(e => e > 4).toList)
  println(stream.takeWhile(e => e < 3).toList)

  println(stream.takeWhile2(e => e > 2).toList)
  println(stream.takeWhile2(e => e < 5).toList)

  // exercise 5.4
  println("*** forAll function ***")
  println(stream.forAll(e => e < 8))
  println(stream.forAll(e => e > 2))

  // exercise 5.5
  println("*** takeWhileViaFoldRight ***")
  println(stream.takeWhileViaFoldRight(e => e % 2 == 1).toList)
  println(stream.takeWhileViaFoldRight(e => e > 2).toList)
  println(stream.takeWhileViaFoldRight(e => e < 1).toList)

  // exercise 5.6
  println("*** headOptionViaFoldRight ***")
  println(stream.headOptionViaFoldRight)

  // exercise 5.7
  println("*** mapViaFoldRight ***")
  println(stream.mapViaFoldRight(e => e * 2).toList)

  println("*** filterViaFoldRight ***")
  println(stream.filterViaFoldRight(e => e > 3).toList)
  println(stream.filterViaFoldRight(e => e > 10).toList)

  println("*** appendViaFoldRight ***")
  println(stream.appendViaFoldRight(Stream(9,2,0)).toList)

  println("*** infinite streams ***")
  println(Stream.ones.mapViaFoldRight(_ + 1).exists(_ % 2 == 0))
  println(Stream.ones.takeWhile2(_ == 1))
  println(Stream.ones.forAll(_ != 1))

  // exercise 5.8
  println(Stream.constant("wow").take(5).toList)

  // exercise 5.9
  println(Stream.from(3).filterViaFoldRight(_ % 10 == 0).take(5).toList)

  // exercise 5.10
  println(Stream.fibs.take(20).toList)

  // exercise 5.11
  println("*** Unfold ***")
  println(Stream.unfold(3)(e => if e * 2 < 200 then Some(e * 2, e * 2) else None).toList)
  println(Stream.unfold(3)(e => if e * 2 > 0 then Some(e * 2, e * 2) else None).take(10).toList)

  // exercise 5.12
  println("*** Unfold fibs ***")
  println(Stream.fibsViaUnfold.take(12).toList)

  println("*** Unfold from ***")
  println(Stream.fromViaUnfold(3).take(10).toList)

  println("*** Unfold constant ***")
  println(Stream.constantViaUnfold(3).take(10).toList)

  println("*** Unfold ones ***")
  println(Stream.onesViaUnfold.take(5).toList)

  // exercise 5.13
  println("*** Unfold map ***")
  println(stream.mapViaUnfold(e => e * 2).toList)

  println("*** Unfold take ***")
  println(stream.takeViaUnfold(3).toList)

  println("*** Unfold takeWhile ***")
  println(stream.takeWhileViaUnfold(e => e % 2 == 1).toList)

  println("*** Unfold zipWith ***")
  println(stream.zipWithViaUnfold(Stream(5, 6, 2))((a, b) => a * b).toList)
  val catStream = Stream("Dalek", "Leia", "Rose", "Massie", "Boe")
  println(catStream.zipWithViaUnfold(Stream("Boss", "Cuddle", "Diva", "Shadow", "Scaredy"))((a, b) => b + "_" + a).toList)

  println("*** Unfold zipAll ***")
  println(stream.zipAll(Stream(12, 0)).toList)
  println(stream.zipAll(Stream(12, 0, 8, 2, 1, 9)).toList)

  // exercise 5.14
  println("*** startsWith ***")
  println(stream.startsWith(Stream(3, 5)))
  println(stream.startsWith(Stream(3, 5, 300)))
  println(stream.startsWith(Stream(3, 5, 6, 1, 7))) // prefix stream is longer than original
  println(Stream.ones.startsWith(Stream(1, 1)))

  // exercise 5.15
  println("*** Unfold tails ***")
  println(stream.tails.mapViaUnfold(_.toList).toList)

  // exercise 5.16
  println("*** scanRight ***")
  println(stream.scanRight(0)(_ + _).toList)
  println(stream.scanRight(3)(_ * _).toList)
  println(catStream.scanRight("^_^")(_ + " & " + _).toList)

}
