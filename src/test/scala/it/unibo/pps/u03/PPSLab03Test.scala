import org.junit.*
import org.junit.Assert.*
import u03.Optionals.Optional.{Empty, Just}
import u03.Sequences.Sequence.{Cons, Nil}

class Task1Test:
  import u03.Sequences.*
  import Sequence.*

  val sequence: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(sequence))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(sequence)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(sequence)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(sequence)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(sequence)(_ != 20))

  @Test def testSkip() =
    assertEquals(Cons(30, Nil()), skip(sequence)(2))
    assertEquals(Nil(), skip(sequence)(3))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), skip(sequence)(0))
    assertEquals(Nil(), skip(Nil())(2))

  @Test def testZip() =
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(sequence, l2))
    assertEquals(Nil(), zip(sequence, Nil()))
    assertEquals(Nil(), zip(Nil(), l2))
    assertEquals(Nil(), zip(Nil(), Nil()))

  @Test def testConcat() =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(sequence, l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))

  @Test def testReverse() =
    assertEquals(Cons(30, Cons(20, Cons(10, Nil()))), reverse(sequence))
    assertEquals(Nil(), reverse(Nil()))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(sequence)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

  @Test def testMin() =
    assertEquals(Just(10), min(sequence))
    assertEquals(Just(1), min(Cons(1, Nil())))
    assertEquals(Empty(), min(Nil()))

  @Test def testEvenIndices() =
    assertEquals(Cons(10, Cons(30, Nil())), evenIndices(sequence))
    assertEquals(Nil(), evenIndices(Nil()))

  @Test def testContains() =
    assertEquals(true, contains(sequence)(10))
    assertEquals(false, contains(sequence)(15))
    assertEquals(false, contains(Nil())(10))

  @Test def testDistinct() =
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), distinct(sequence))
    assertEquals(Cons(20, Cons(10, Nil())), distinct(Cons(10, Cons(20, Cons(10, Nil())))))
    assertEquals(Nil(), distinct(Nil()))

class Task2Test:
  import u03.Sequences.*
  import it.unibo.pps.u03.Task2.*
  import Person.*

  @Test def testGetCourser() =
    val s = Cons(Teacher("Viroli", "PPS"), Cons(Teacher("Bravetti", "LCMC"), Cons(Student("Nicholas Forlivesi", 2002), Nil())))
    assertEquals(Cons("PPS", Cons("LCMC", Nil())), getCourses(s))

  @Test def testFoldLeft() =
    assertEquals(60, foldLeft(Cons(2, Cons(5, Cons(3, Nil()))))(2)(_*_))
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_-_)) // -16

  @Test def testGetNDistinctCourses() =
    val lst = Cons(Teacher("Viroli", "PPS"), Cons(Student("Nicholas", 2002), Cons(Teacher("Aguzzi", "PPS"), Cons(Teacher("Ricci", "PCD"), Nil()))))
    assertEquals(2, getNDistinctCourses(lst))

class Task3Test:
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