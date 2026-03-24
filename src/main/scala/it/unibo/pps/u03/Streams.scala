package u03

object Streams extends App :

  import Sequences.*

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

@main def tryStreams =
  import Streams.* 

  val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
  val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
  val str3 = Stream.filter(str2)(x => (x < 3 || x > 20)) // {1,2,21,22,..}
  val str4 = Stream.take(str3)(10) // {1,2,21,22,..,28}
  println(Stream.toList(str4)) // [1,2,21,22,..,28]

  lazy val corec: Stream[Int] = Stream.cons(1, corec) // {1,1,1,..}
  println(Stream.toList(Stream.take(corec)(10))) // [1,1,..,1]