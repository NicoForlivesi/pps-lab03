package it.unibo.pps.u03

import u03.Optionals.Optional
import u03.Sequences.Sequence
import u03.Sequences.Sequence.{Cons, Nil, distinct, filter, map}

import scala.annotation.tailrec

// Task 1
object Sequences:

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = (s, n) match
      case (_, 0) => s
      case (Cons(_, t), _) => skip(t)(n - 1)
      case (Nil(), _) => Nil()

    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Nil(), _) => Nil()
      case (_, Nil()) => Nil()
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), (zip(t1, t2)))

    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = (s1, s2) match
      case (Nil(), _) => s2
      case (_, Nil()) => s1
      case (Cons(h1, Nil()), _) => Cons(h1, s2)
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1, concat(t1, s2))

    def reverse[A](s: Sequence[A]): Sequence[A] = s match
      case Nil() => Nil()
      case Cons(h, t) => concat(reverse(t), Cons(h, Nil()))

    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
      case Nil() => Nil()
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper));

    def min(s: Sequence[Int]): Optional[Int] = s match
      case Nil() => Optional.Empty()
      case Cons(h, Nil()) => Optional.Just(h)
      case Cons(h, t) => min(t) match
        case Optional.Empty() => Optional.Just(h)
        case Optional.Just(m) if m < h => Optional.Just(m)
        case _ => Optional.Just(h)

    def evenIndices[A](s: Sequence[A]): Sequence[A] = s match
      case Nil() => Nil()
      case Cons(_, Nil()) => s
      case Cons(h, Cons(_, t)) => Cons(h, evenIndices(t))

    def contains[A](s: Sequence[A])(elem: A): Boolean = s match
      case Nil() => false
      case Cons(h, _) if h == elem => true
      case Cons(_, t) => contains(t)(elem)

    def distinct[A](s: Sequence[A]): Sequence[A] = s match
      case Nil() => Nil()
      case Cons(h, t) if contains(t)(h) => distinct(t)
      case Cons(h, t) => Cons(h, distinct(t))
      // Allora così funziona ma per elementi multipli viene tenuto l'ultimo che si incontra invece
      // per come è fatto il test va tenuto il primo, c'è solo un problem sull'ordine quindi

object Task2:
  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:

    def name(p: Person): String = p match
      case Student(n, _) => n
      case Teacher(n, _) => n

    private def isTeacher(p: Person): Boolean = p match
      case Teacher(_, _) => true
      case _ => false

    private def getTeacherCourse(p: Person): String = p match
      case Teacher(_, course) => course

    def getCourses(s: Sequence[Person]): Sequence[String] =
      map(filter(s)(isTeacher))(getTeacherCourse)

    private def countElements[A](s: Sequence[A]): Int = s match
      case Nil() => 0
      case Cons(h, t) => 1 + countElements(t)

    private def tailRecursiveCountElements[A](s: Sequence[A]): Int =
      @tailrec
      def _tailRecCountElements[B](s: Sequence[B])(currentCount: Int): Int = s match
        case Nil() => currentCount
        case Cons(h, t) => _tailRecCountElements(t)(currentCount + 1)
      _tailRecCountElements(s)(0)

    def getNDistinctCourses(s: Sequence[Person]): Int =
      def getDistinctCoursesSequence(s: Sequence[Person]): Sequence[String] =
        distinct(getCourses(s))
      tailRecursiveCountElements(getDistinctCoursesSequence(s))

  @tailrec
  def foldLeft[A](s: Sequence[Int])(defaultV: A)(f: (A, Int) => A): A = s match
    case Nil() => defaultV
    case Cons(h, t) => foldLeft(t)(f(defaultV, h))(f)

// Task 3
object Streams:

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case _ => Empty()

    def fill[A](n: Int)(element: => A): Stream[A] = n match
      case 0 => Empty()
      case _ => cons(element, fill(n - 1)(element))

    def fibonacci(): Stream[Int] =
      def _fib(n1: Int, n2: Int): Stream[Int] = (n1, n2) match
        case (0, 1) => cons(0, cons(1, cons(1, _fib(n2, n1 + n2))))
        case _ => cons(n1 + n2, _fib(n2, n1 + n2))
      _fib(0, 1)

    def fromList[A](s: Sequence[A]): Stream[A] = s match
      case Sequence.Cons(h, t) => cons(h, fromList(t))
      case Sequence.Nil() => Empty()

    def interleave[A, B](s1: Stream[A], s2: Stream[B]): Stream[A | B] = (s1, s2) match
      case (Cons(h1, t1), Cons(h2, t2)) => cons(h1(), interleave(s2, t1()))
      case (Cons(h1, t1), Empty()) => s1.asInstanceOf[Stream[A | B]]
      case (Empty(), Cons(h2, t2)) => s2.asInstanceOf[Stream[A | B]]
      case _ => Empty()

    def cycle[A](lst: Sequence[A]): Stream[A] =
      def _cycle(dummy: Sequence[A]): Stream[A] = dummy match
        case Sequence.Cons(h, t) => cons(h, _cycle(t))
        case Sequence.Nil() => _cycle(lst)
      _cycle(lst)

  end Stream
