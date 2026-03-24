package it.unibo.pps.u03

import u03.Sequences.*
import u03.Sequences.Sequence.*

import scala.annotation.tailrec

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
