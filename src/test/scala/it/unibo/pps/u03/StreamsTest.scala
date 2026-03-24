package it.unibo.pps.u03

import org.junit.*
import org.junit.Assert.*
import u03.Sequences

class StreamsTest:
  import Sequences.Sequence.*
  import u03.Streams.*

  @Test def testTakeWhile() =
    val stream = Stream.iterate(0)(_ + 1)
    val actual = Stream.toList(Stream.takeWhile(stream)(_ < 5))
    val expected = Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil())))))
    assertEquals(expected, actual)

  @Test def testFill() =
    val actual = Stream.toList(Stream.fill(3)("a"))
    val expected = Cons("a", Cons("a", Cons("a", Nil())))
    assertEquals(expected, actual)

  @Test def testFibonacci() =
    val actual = Stream.toList(Stream.take(Stream.fibonacci())(6))
    val expected = Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Nil()))))))
    assertEquals(expected, actual)

  @Test def testFromList() =
    val s1 = Stream.toList(Stream.fromList(Cons(1, Cons(2, Cons(3, Nil())))))
    assertEquals(Cons(1, Cons(2, Cons(3, Nil()))), s1)

  @Test def testInterleave() =
    val s1 = Stream.fromList(Cons("a", Cons("b", Cons("c", Nil()))))
    val s2 = Stream.fromList(Cons(2, Cons(4, Cons(6, Cons(8, Cons(10, Nil()))))))
    val actual = Stream.toList(Stream.interleave(s1, s2))
    val expected = Cons("a", Cons(2, Cons("b", Cons(4, Cons("c", Cons(6, Cons(8, Cons(10, Nil()))))))))
    assertEquals(expected, actual)

  @Test def testCycle() =
    val lst = Cons("a", Cons("b", Cons("c", Nil())))
    val expected = Cons("a", Cons("b", Cons("c", Cons("a", Cons("b", Nil())))))
    val actual = Stream.toList(Stream.take(Stream.cycle(lst))(5))
    assertEquals(expected, actual)
