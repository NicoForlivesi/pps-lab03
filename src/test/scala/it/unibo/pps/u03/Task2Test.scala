package it.unibo.pps.u03

import org.junit.*
import org.junit.Assert.*

class Task2Test:
  import u03.Sequences.*
  import Sequence.*
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
