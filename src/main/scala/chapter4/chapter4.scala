package chapter4

def Try[A](a: => A): Option[A] =
  try Some(a)
  catch {
    case e: Exception => None
  }

trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match
      case None => None
      case Some(value) => Some(f(value))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f).getOrElse(None)

//    this match
//      case None => None
//      case Some(value) => f(value)
  }

  def getOrElse[B >: A](default: => B): B = {
    this match
      case None => default
      case Some(value) => value
  }

  def orElse[B >: A](alt: => Option[B]): Option[B] = {
    this match
      case None => alt
      case Some(_) => this

    //map(Some(_)).getOrElse(alt)
  }

  def filter(f: A => Boolean): Option[A] = {
    this match
      case Some(value) if f(value) => Some(value)
      case _ => None

    //flatMap(a => if f(a) then Some(a) else None)
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

def TryEither[A](a: => A): Either[Exception, A] =
  try Right(a)
  catch { case e: Exception => Left(e) }

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = {
    this match
      case Right(value) => Right(f(value))
      case Left(value) => Left(value)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match
      case Right(value) => f(value)
      case Left(value) => Left(value)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match
      case Right(_) => this
      case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    (this, b) match
      case (_, Left(e)) => Left(e)
      case (Left(e), _) => Left(e)
      case (Right(val_a), Right(val_b)) => Right(f(val_a, val_b))
  }

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

@main def main() : Unit = {

  // exercise 4.1
  val some_option : Option[Int] = Some(4)
  val none_option : Option[Int] = None
  val list_option : Option[List[Int]] = Some(List(1, 2, 3, 4, 5, 6))

  println("*** test map ***")
  println(some_option.map(e => e * 2))
  println(none_option.map(e => e * 2))
  println(list_option.map(e => e.map(_ - 100)))

  println("*** test getOrElse ***")
  println(some_option.getOrElse(0))
  println(none_option.getOrElse(0))

  println("*** test flatmap ***")
  println(some_option.flatMap(e => Some(e * 3)))
  println(none_option.flatMap(e => Some(e * 3)))
  println(some_option.flatMap(_ => None))

  println("***test orElse ***")
  println(some_option.orElse(Some(100)))
  println(some_option.orElse(None))
  println(none_option.orElse(Some(0)))
  println(none_option.orElse(None))

  println("*** test filter ***")
  println(some_option.filter(e => e > 5))
  println(some_option.filter(e => e > 3))
  println(none_option.filter(e => e > 5))


  // exercise 4.2
  println("*** variance function ***")
  println(variance(List(1.2, 3.4, 7.01)))
  println(variance(List.empty))

  // exercise 4.3
  println("*** Map2 function ***")
  println(parseInsuranceRateQuote("70", "20"))
  println(parseInsuranceRateQuote("hundred", "20"))

  // exercise 4.4
  println("*** Sequence function ***")
  println(sequence2(List(Some("a"), Some("b"), None, Some("c"))))
  println(sequence2(List(Some("a"), Some("b"), Some("c"))))

  // exercise 4.5
  println("*** Traverse function ***")
  println(traverse(List(11, 25, 57))(a => if(a < 10) then None else Some(a - 10)))
  println(traverse(List(11, 5, 57))(a => if(a < 10) then None else Some(a - 10)))

  // exercise 4.6
  val right_either: Either[Exception, Int] = Right(5)
  val left_either: Either[Exception, Int] = Left(Exception("Oops"))

  println("*** Either Functions ***")
  println("*** test map ***")
  println(right_either.map(e => e * 3))
  println(left_either.map(e => e * 3))

  println("*** test flatmap ***")
  println(right_either.flatMap(e => Right(e * 3)))
  println(left_either.flatMap(e => Right(e * 3)))

  println("***test orElse ***")
  println(right_either.orElse(Right(3)))
  println(left_either.orElse(Right(3)))
  println(left_either.orElse(Left(Exception("BAD!"))))

  println("*** Map2 function ***")
  println(right_either.map2(Right(3))((a, b) => a * b))
  println(left_either.map2(Right(3))((a, b) => a * b))

  // exercise 4.7
  println("*** Sequence function ***")
  println(sequenceEither(List(Right("a"), Right("b"), Right("c"))))
  println(sequenceEither(List(Right("a"), Right("b"), Left(Exception("NOT OK")), Right("c"))))

  println("*** Traverse function ***")
  println(traverseEither(List(11, 25, 57))(a => if (a < 10) then Left(Exception("L'Erreur")) else Right(a - 10)))
  println(traverseEither(List(11, 5, 57))(a => if (a < 10) then Left(Exception("L'Erreur")) else Right(a - 10)))


}

def mean(xs: Seq[Double]): Option[Double] = {
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)
}

def variance(xs: Seq[Double]): Option[Double] = {
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}

def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  (a, b) match
    case (_, None) => None
    case (None, _) => None
    case (Some(val_a), Some(val_b)) => Some(f(val_a, val_b))
}

def insuranceRateQuote(age: Int, tickets: Int): Int = {
  age * tickets
}

def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
  val optAge: Option[Int] = Try {
    age.toInt
  }
  val optTickets: Option[Int] = Try {
    numberOfSpeedingTickets.toInt
  }
  map2(optAge, optTickets)(insuranceRateQuote)
}


def sequence[A](a: List[Option[A]]): Option[List[A]] = {
  a match
    case Nil => Some(Nil)
    case h :: t => h.flatMap(hh => sequence(t).map(e => hh :: e))
}

def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
  def go(lst: List[Option[A]], acc: List[A]): Option[List[A]] = {
    lst match
      case head :: tail => head match
        case Some(value) => go(tail, acc :+ value)
        case None => None
      case Nil => Some(acc) // end of the list, didnt encounter any 'None'
  }
  go(a, Nil)
}

def sequence3[A](lst: List[Option[A]]): Option[List[A]] = {
  lst match
    case head :: tail => map2(head, sequence3(tail))((head_value, tail_value) => head_value :: tail_value)
    case Nil => Some(Nil)
}


def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
  def go(a: List[A], acc: List[B]): Option[List[B]] = {
    a match
      case Nil => Some(acc)
      case head :: tail => {
        f(head) match
          case Some(value) => go(tail, acc :+ value)
          case None => None
      }
  }
  go(a, Nil)
}


def sequenceEither[E, A](l: List[Either[E, A]]): Either[E, List[A]] = {
  def go(lst: List[Either[E, A]], acc: List[A]): Either[E, List[A]] = {
    lst match
      case head :: tail => head match
        case Right(value) => go(tail, acc :+ value)
        case Left(e) => Left(e)
      case Nil => Right(acc) // end of the list, didnt encounter any 'Left'
  }
  go(l, Nil)
}

def traverseEither[E, A, B](l: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
  def go(l: List[A], acc: List[B]): Either[E, List[B]] = {
    l match
      case head :: tail => {
        f(head) match
          case Right(value) => go(tail, acc :+ value)
          case Left(e) => Left(e)
      }
      case Nil => Right(acc)
  }

  go(l, Nil)
}


case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

def mkName(name: String): Either[String, Name] =
  if (name == "" || name == null) Left("Name is empty.")
  else Right(new Name(name))
def mkAge(age: Int): Either[String, Age] =
  if (age < 0) Left("Age is out of range.")
  else Right(new Age(age))
def mkPerson(name: String, age: Int): Either[String, Person] =
  mkName(name).map2(mkAge(age))(Person(_, _))

