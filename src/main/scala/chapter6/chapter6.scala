package chapter6
trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
def randomPair(rng: RNG): ((Int,Int), RNG) = {
  val (i1,rng2) = rng.nextInt
  val (i2,rng3) = rng2.nextInt
  ((i1,i2), rng3)
}
def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val value = rng.nextInt
  value match
    case (i, r) if(i == Int.MinValue) => r.nextInt
    case (i, r) if(i < 0) => (i * -1, r)
    case _ => value
}
def double(rng: RNG): (Double, RNG) = {
  nonNegativeInt(rng.nextInt._2) match
    case (i, r) => (((i % 10000).toDouble / 10000), r)
}

def intDouble(rng: RNG): ((Int,Double), RNG) = {
  val (v, r) = rng.nextInt
  ((v, double(rng)._1), r)
}

def doubleInt(rng: RNG): ((Double,Int), RNG) = {
  val (v, r) = rng.nextInt
  ((double(rng)._1, v), r)
}

def double3(rng: RNG): ((Double,Double,Double), RNG) = {
  val (d1, r1) = double(rng)
  val (d2, r2) = double(r1)
  val (d3, r3) = double(r2)

  ((d1, d2, d3), r3)
}

def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  def go(n: Int, rng: RNG, list: List[Int]): (List[Int], RNG) = {
    if (n <= 0) (list, rng)
    else {
      val (i, r) = rng.nextInt
      go(n - 1, r, i +: list)
    }
  }
  go(count, rng, Nil)
}


type Rand[+A] = RNG => (A, RNG)

def unit[A](a: A): Rand[A] = {
  rng => (a, rng)
}

def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
  rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }
}

def nonNegativeEven: Rand[Int] = {
  map(nonNegativeInt)(i => i - i % 2)
}

def doubleViaMap: Rand[Double] = {
  map(nonNegativeInt)(e => (e % 10000).toDouble / 10000)
}

def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
  rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }
}

def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = {
  map2(ra, rb)((_, _))
}

val randIntDouble: Rand[(Int, Double)] = {
  both(_.nextInt, double)
}

val randDoubleInt: Rand[(Double, Int)] = {
  both(double, _.nextInt)
}

def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
  rng => {
    def go(lst: List[Rand[A]], acc: (List[A], RNG)) : (List[A], RNG) = {
      lst match
        case Nil => acc
        case (h :: t) => {
          val (a, newRNG) = h(acc._2)
          go(t, (acc._1 :+ a, newRNG))
        }
    }
    go(fs, (Nil, rng))
  }
}

def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
  rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }
}

def nonNegativeLessThan(n: Int): Rand[Int] = {
  flatMap(nonNegativeInt)(e => {
    val mod = e % n
    if (e + (n - 1) - mod >= 0) rng => (mod, rng)
    else nonNegativeLessThan(n)
  })
}

def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = {
  flatMap(s)(a => rng => (f(a), rng))
}

def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
  flatMap(ra)(a => map(rb)(b => f(a, b)))
}

type Rand2[A] = State[RNG, A]

object State {
  def unit[S, A](a: A): S => (A, S) = s => (a, s)

  def sequence[S, A](fs: List[S => (A, S)]): S => (List[A], S) = {
    s => {
      def go(lst: List[S => (A, S)], acc: (List[A], S)): (List[A], S) = {
        lst match
          case Nil => acc
          case (h :: t) => {
            val (a, newState) = h(acc._2)
            go(t, (acc._1 :+ a, newState))
          }
      }

      go(fs, (Nil, s))
    }
  }

  def modify[S](f: S => S): State[S, Unit] = State(for {
    s <- get
    _ <- set(f(s))
  } yield ())

  def modify2[S](f: S => S): State[S, Unit] = {
    State(get.flatMap(s => set(f(s)).map(_ => ())))
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}

case class State[S,+A](run: S => (A,S)) {
  def map[B](f: A => B): S => (B,S) = {
    s => {
      val (a, s2) = run(s)
      (f(a), s2)
    }
  }

  def flatMap[B](g: A => S => (B,S)): S => (B,S) = {
    s => {
      val (a, s2) = run(s)
      g(a)(s2)
    }
  }

  def mapViaFlatMap[B](f: A => B): S => (B,S) = {
    flatMap(a => State.unit(f(a)))
  }

  def map2[B, C](rb: S => (B,S))(f: (A, B) => C): S => (C,S) = {
    s => {
      val (a, s2) = run(s)
      val (b, s3) = rb(s2)
      (f(a, b), s3)
    }
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)

def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
  State(State(State(State.sequence(inputs.map(i => processInput(i).run))).flatMap(_ => State.get.run)).map(m => (m.candies, m.coins)))
}

def processInput(input: Input): State[Machine, Unit] = {
  State.modify(m => {
    input match
      case Coin if(m.locked && m.candies > 0) => Machine(false, m.candies, m.coins + 1)
      case Turn if(!m.locked) => Machine(true, m.candies - 1, m.coins)
      case _ => m
  })
}

@main def main() : Unit = {

  val rng = SimpleRNG(42)
  val (n1, rng2) = rng.nextInt
  val (n2, rng3) = rng2.nextInt

  println(n1 + " " + n2)
  println(randomPair(rng))

  //exercise 6.1
  println("*** Non Negative Int ***")
  println(nonNegativeInt(rng)._1)
  println(nonNegativeInt(SimpleRNG(392123))._1)

  //exercise 6.2
  println("*** random double ***")
  println(double(rng)._1)
  println(double(SimpleRNG(823774))._1)

  //exercise 6.3
  println("*** random pairs ***")
  println(intDouble(rng)._1)
  println(doubleInt(rng)._1)
  println(double3(rng)._1)

  //exercise 6.4
  println("*** list of random ints ***")
  println(ints(3)(rng)._1)
  println(ints(10)(SimpleRNG(2))._1)

  val R: Rand[Int] = _.nextInt
  println(R(rng))
  println(map(R)(e => e * e)(rng)._1)


  //exercise 6.5
  println("*** random double via map ***")
  println(doubleViaMap(rng)._1)

  //exercise 6.6
  println("*** Map2 ***")
  println(map2(R, R)((a, b) => a + b)(rng)._1)

  println(randIntDouble(rng)._1)
  println(randDoubleInt(rng)._1)

  //exercise 6.7
  println("*** Sequence ***")
  println(sequence(List.fill(5)(R))(rng)._1)

  //exercise 6.8
  println("*** FlatMap ***")
  println(nonNegativeLessThan(50)(rng)._1)
  println(nonNegativeLessThan(100)(SimpleRNG(Int.MaxValue))._1)

  //exercise 6.9
  println("*** Map via flatMap ***")
  println(map(R)(e => e * e)(rng)._1)
  println(mapViaFlatMap(R)(e => e * e)(rng)._1)

  println("*** Map2 via flatMap ***")
  println(map2(R, R)((a, b) => a + b)(rng)._1)
  println(map2ViaFlatMap(R, R)((a, b) => a + b)(rng)._1)

  def rollDie: Rand[Int] = nonNegativeLessThan(6)
  println("rolldie: " + rollDie(rng)._1)
  println("rolldie: " + rollDie(SimpleRNG(5))._1) // results in zero

  def rollDie2: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1) // prevents 0, range 1-6
  println("rolldie2: " + rollDie2(rng)._1)
  println("rolldie2: " + rollDie2(SimpleRNG(5))._1)

  //exercise 6.10
  println("*** General state class ***")
  val R2: Rand2[Int] = State(_.nextInt)

  println("*** state map ***")
  println(map(R)(e => e * e)(rng)._1)
  println(R2.map(e => e * e)(rng)._1)
  val R3: Rand2[Int] = State(R2.map(e => e * e))
  println(R3.map(e => e * e)(rng)._1)

  println("*** state flatmap (via map) ***")
  println(R2.mapViaFlatMap(e => e * e)(rng)._1)

  println("*** state map2 ***")
  println(map2(R, R)((a, b) => a + b)(rng)._1)
  println(R2.map2(R2.run)((a, b) => a + b)(rng)._1)
  println(R3.map2(R2.run)((a, b) => a + b)(rng)._1)
  println(R2.map2(R3.run)((a, b) => a + b)(rng)._1)

  println("*** state sequence ***")
  println(sequence(List.fill(5)(R))(rng)._1)
  println(State.sequence(List.fill(5)(R2.run))(rng)._1)
  println(State.sequence(List.fill(20)(R3.run))(rng)._1)


  println("*** state modify ***")
  println(State.modify[RNG](e => nonNegativeInt(e)._2).run(rng))
  println(State.modify2[RNG](e => nonNegativeInt(e)._2).run(rng))

  println("*** Finite State Automaton ~ Candy Dispenser ***")

  val machine = Machine(true, 6, 0)
  val inputs1: List[Input] = List(Coin, Turn, Coin, Turn, Turn, Coin, Turn)

  val result1 = processInput(Coin).run(machine)
  println(result1._2)

  val result2 = simulateMachine(inputs1).run(machine)._1
  println(s"Remaining candies: ${result2._1} Total coins ${result2._2}" )

  val inputs2: List[Input] = List(Turn, Coin, Coin, Turn, Coin, Coin, Turn, Turn, Turn)
  val result3 = simulateMachine(inputs2).run(machine)._1
  println(s"Remaining candies: ${result3._1} Total coins ${result3._2}")

  val machine2 = Machine(true, 2, 0)
  val inputs3: List[Input] = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
  val result4 = simulateMachine(inputs3).run(machine2)._1
  println(s"Remaining candies: ${result4._1} Total coins ${result4._2}")

  val result5 = simulateMachine(List.empty).run(machine2)._1
  println(s"Remaining candies: ${result5._1} Total coins ${result5._2}")

}


