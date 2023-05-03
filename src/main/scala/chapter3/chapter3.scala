package chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // exercise 3.2
  def tail[A](l : List[A]) : List[A] = l match {
    case Nil => Nil
    case Cons(_,t) => t
  }

  // exercise 3.3
  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  // exercise 3.4
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => {
      if(n > 0) drop(t, n - 1)
      else l
    }
  }

  // exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => {
      if(f(h)) dropWhile(t, f)
      else l
    }
  }

  // exercise 3.8
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // exercise 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, y) => y + 1)
  }

  // exercise 3.10
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // exercise 3.11
  def sum2(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)

  def product2(ns: List[Int]) =
    foldLeft(ns, 1)((x, y) => x * y)

  def lengthList(ns: List[Int]) =
    foldLeft(ns, 0)((x, _) => x + 1)

  // exercise 3.12
  def reverseList[A](ns: List[A]) =
    foldLeft[A, List[A]](ns, Nil)((l, h) => Cons(h, l))

  // exercise 3.13
  def foldRightViaFoldLeft[A, B](l: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverseList(l), acc) ((b, a) => f(a, b))

  def foldRightViaFoldLeft_1[A, B](l: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(l, (b: B) => b) ((g, a) => b => g(f(a, b)))(acc)

  def foldLeftViaFoldRight[A, B](l: List[A], acc: B, f: (B, A) => B): B =
    foldRight(l, (b: B) => b) ((a, g) => b => g(f(b, a)))(acc)

  // exercise 3.14
  def append[A](l: List[A], r: List[A]) : List[A] =
    foldRight(l, r)(Cons(_, _))

  def concat[A](l : List[List[A]]) : List[A] =
    foldLeft(l, Nil : List[A])(append)

  // exercise 3.16
  def addOne(l: List[Int]) : List[Int] = {
    def go(l: List[Int], n: List[Int]): List[Int] = {
      l match
        case Nil => n
        case Cons(h, t) => go(t, append(n, Cons(h + 1, Nil)))
    }

    go(l, Nil: List[Int])
  }

  // exercise 3.17
  def doubleToString(l: List[Double]): List[String] = {
    def go(l: List[Double], n: List[String]): List[String] = {
      l match
        case Nil => n
        case Cons(h, t) => go(t, append(n, Cons(h.toString, Nil)))
    }

    go(l, Nil: List[String])
  }

  // exercise 3.18
  def map[A, B](l: List[A])(f: A => B): List[B] = {
    def go(l: List[A], result: List[B]) : List[B] =
      l match {
        case Nil => result
        case Cons(h, t) => go(t, append(result, Cons(f(h), Nil)))
      }

    go(l, Nil: List[B])
  }

  // exercise 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    def go(l: List[A], result: List[A]): List[A] =
      l match {
        case Nil => result
        case Cons(h, t) if f(h) => go(t, append(result, Cons(h, Nil)))
        case Cons(_, t) => go(t, result)
      }

    go(l, Nil: List[A])
  }

  // exercise 3.20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    concat(map(l)(f))
  }

  // exercise 3.21
  def flatmapFilter[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(e => {
      if f(e) then List(e)
      else Nil
    })
  }

  // exercise 3.22
  def listAddition(l1: List[Int], l2: List[Int]) : List[Int] = {
    def go(l1: List[Int], l2: List[Int], result: List[Int]) : List[Int] = {
      (l1,l2) match
        case (Nil,_) => result
        case (_, Nil) => result
        case (Cons(h1,t1), Cons(h2,t2)) => go(t1, t2, append(result, Cons(h1 + h2, Nil)))
    }

    go(l1,l2, Nil)
  }

  // exercise 3.23
  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = {
    def go(l1: List[A], l2: List[A], result: List[A]): List[A] = {
      (l1, l2) match
        case (Nil, _) => result
        case (_, Nil) => result
        case (Cons(h1, t1), Cons(h2, t2)) => go(t1, t2, append(result, Cons(f(h1,h2), Nil)))
    }

    go(l1, l2, Nil)
  }


  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def go(sup: List[A], sub: List[A]): Boolean = {
      (sup, sub) match
        case (Nil, Cons(_, _)) => false
        case (_, Nil) => true
        case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => go(t1, t2)
        case (Cons(_, _), Cons(_, _)) => false
    }

    sup match
      case Nil => false
      case Cons(h, t) if go(sup, sub) => true
      case Cons(h, t) => hasSubsequence(t, sub)
  }
}


@main def main() : Unit = {
  // exercise 3.1
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
//    case _ => 101
  }

  println("x match: " + x)

  println("*** exercise 3.2 ***")
  println(List.tail(List(2, 5, 7)))
  println(List.tail(Nil))

  println("*** exercise 3.3 ***")
  println(List.setHead("cats", List("dogs", "party", "hard")))
  println(List.setHead("Empty No more", Nil))


  println("*** exercise 3.4 ***")
  println(List.drop(List(3, 5, 6, 8, 2), 2))
  println(List.drop(List(3, 5, 6, 8, 2), 0))
  println(List.drop(List(3, 5, 6, 8, 2), 10))


  println("*** exercise 3.5 ***")
  println(List.dropWhile(List(3, 5, 7, 2, 9), x => x < 6))
  println(List.dropWhile(List("cats", "cows", "lizards", "lions"), x => x.startsWith("c")))
  println(List.dropWhile(List(6, 8, 9), x => x < 6))
  println(List.dropWhile[Int](Nil, x => x != 0))


  println("*** exercise 3.8 ***")
  println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))


  println("*** exercise 3.9 ***")
  println(List.length(List("hello", "world", "!")))
  println(List.length(Nil))

  // make leftFold tail recursive
  println("*** exercise 3.10 ***")
  println(List.sum2(List(3, 4, 2)))


  println("*** exercise 3.11 ***")
  println(List.sum2(List(3, 4, 2)))
  println(List.product2(List(3, 4, 2)))
  println(List.product2(Nil))
  println(List.lengthList(List(3, 4, 2)))
  println(List.lengthList(Nil))

  println("*** exercise 3.12 ***")
  println(List.reverseList(List(3, 4, 9)))
  println(List.reverseList(List("Dalek", "Leia", "Rose", "Massie", "Boe")))

  println("*** exercise 3.14 ***")
  println(List.append(List(3, 4, 9), List(1,2,7)))

  println("*** exercise 3.15 ***")
  println(List.concat(List(List(3, 4, 9), List(1, 2, 7), List(0,8))))


  println("*** exercise 3.16 ***")
  val l16 = List[Int] (3, 4, 9)
  println("Before " + l16)
  println("After " + List.addOne(l16))


  println("*** exercise 3.17 ***")
  val l17 = List[Double](0.562, 7.21, -3.4)
  println("Before " + l17)
  println("After " + List.doubleToString(l17))


  println("*** exercise 3.18 ***")
  val l18 = List[Int](1, 2, 3)
  println("Before " + l18)
  println("After " + List.map(l18)(e => e + 1))

  println("*** exercise 3.19 ***")
  val l19 = List[Int](1, 2, 3, 4, 5, 6, 7, 8)
  println("Before " + l19)
  println("After " + List.filter(l19)(e => e % 2 == 0))

  println("*** exercise 3.20 ***")
  println(List.flatMap(List(1,2,3))(i => List(i,i)))


  println("*** exercise 3.21 ***")
  val l22 = List[Int](1, 2, 3, 4, 5, 6, 7, 8)
  println("Before " + l22)
  println("After " + List.flatmapFilter(l22)(e => e % 2 != 0))


  println("*** exercise 3.22 ***")
  println(List.listAddition(List(1, 2, 3), List(5, 6, 2)))
  println(List.listAddition(List(1, 2, 3, 7), List(5, 6, 2))) // uneven lists

  println("*** exercise 3.23 ***")
  println(List.zipWith(List(1, 2, 3), List(5, 6, 2))((a, b) => a * b))
  println(List.zipWith(List("Dalek", "Leia", "Rose", "Massie", "Boe"),
    List("Boss", "Cuddle", "Diva", "Shadow", "Scaredy"))((a, b) => b + "_" + a))

  println("*** exercise 3.24 ***")
  println(List.hasSubsequence(List(1,2,3,4), List(3,4)))
  println(List.hasSubsequence(List(1,2,3,4), List(8)))
  println(List.hasSubsequence(List(1,2,3,4), List(1,2,4)))
  println(List.hasSubsequence(List(1,2,3,4), List(2)))
  println(List.hasSubsequence(List(1,2,3,4), List(3,4,5)))


  println("*** exercise 3.25 ***")
  val tree = Branch(Branch(Leaf(1),Leaf(6)), Branch(Leaf(9), Leaf(7)))
  val crooked_tree = Branch(Branch(Branch(Leaf(1), Leaf(8)),Leaf(6)), Branch(Leaf(9), Leaf(7)))
  println("Size " + Tree.size(tree))

  println("*** exercise 3.26 ***")
  println("Max value " + Tree.maximum(tree))

  println("*** exercise 3.27 ***")
  println("Depth " + Tree.depth(tree))
  println("Depth " + Tree.depth(crooked_tree))

  println("*** exercise 3.28 ***")
  println("Before Map " + tree)
  println("After Map " + Tree.map(tree)(e => e * 2))


  println("*** exercise 3.29 ***")
  println("Size " + Tree.sizeFold(tree))
  println("Max value " + Tree.maximumFold(tree))
  println("Depth " + Tree.depthFold(crooked_tree))
  println("Map " + Tree.mapFold(tree)(e => e * 2))

}


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{

  def size[A](t: Tree[A]) : Int = {
    t match
      case Leaf(_) => 1
      case Branch(l,r) => size(l) + size(r) + 1
  }

  def maximum(t: Tree[Int]) : Int = {
    t match
      case Leaf(value) => value
      case Branch(l, r) => maximum(l).max(maximum(r))
  }

  def depth[A](t: Tree[A]): Int = {
    t match
      case Leaf(_) => 0
      case Branch(l,r) => depth(l).max(depth(r)) + 1
  }

  def map[A, B](t: Tree[A])(f: A => B) : Tree[B] = {
    t match
      case Leaf(value) => Leaf(f(value))
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B,B) => B) : B = {
    t match
      case Leaf(value) => f(value)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeFold[A](t: Tree[A]): Int = {
    fold(t)(e => 1)((a, b) => a + b + 1)
  }

  def maximumFold(t: Tree[Int]): Int = {
    fold(t)(e => e)((a, b) => a max b)
  }

  def depthFold[A](t: Tree[A]): Int = {
    fold(t)(e => 0)((a, b) => (a max b) + 1)
  }

  def mapFold[A, B](t: Tree[A])(f: A => B) : Tree[B] = {
    fold(t)(e => Leaf(f(e)):Tree[B])((a, b) => Branch(a,b)) // cast leaf to tree, otherwise B is set to type 'Leaf'
    // and does not accept 'branch' as type B as well
  }

}


