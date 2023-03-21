package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*
import u03.Lists.Person.{Student, Teacher}

/**
 * This is a test class for the Lists module.
 */
class ListTest:
  import List.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum(): Unit =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_+1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(x => x+""))

  @Test def testFilter(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_>=20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_!=20))

  @Test def testDrop(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l,1))
    assertEquals(Cons(30, Nil()), drop(l,2))
    assertEquals(Nil(), drop(l,5))


  @Test def testAppend(): Unit =
    val tail = Cons(40, Nil())
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40,Nil())))), append(l,tail))
    assertEquals(Nil(), append(Nil(),Nil()))
    assertEquals(l, append(l,Nil()))
    assertEquals(tail, append(Nil(),tail))

  @Test def testFlatMap(): Unit =
    val l1 = Cons(10,Nil())
    assertEquals(Cons(11,Nil()), flatMap(l1)(v => Cons(v + 1,Nil())))
    assertEquals(Cons(11,Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1,Nil())))
    assertEquals(Cons(11,Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))),
      flatMap(l)(v => Cons (v + 1, Cons (v + 2, Nil ()))))
    assertEquals(l, flatMap(l)(v => Cons(v, Nil())))

  @Test def testMax(): Unit =
    assertEquals(Option.Some(50),max(Cons(10,Cons(50,Cons(20, Cons(10, Nil()))))))
    assertEquals(Option.Some(50),max(Cons(50,Cons(10,Cons(20, Cons(10, Nil()))))))
    assertEquals(Option.Some(50),max(Cons(10,Cons(10,Cons(20, Cons(50, Nil()))))))
    assertEquals(Option.Some(0),max(Cons(0,Cons(0,Cons(0, Cons(0, Nil()))))))
    assertEquals(Option.None(), max(Nil()))

  @Test def testGetCoursesByList(): Unit =
    val studentOne = Student("David",1996)
    val studentTwo = Student("Lorenzo",1999)
    val teacherOne = Teacher("Mirko","PPS")
    val teacherTwo = Teacher("Alessandro","PCD")
    val listOfPersons = Cons(studentOne, Cons(teacherOne, Cons(studentTwo, Cons(teacherTwo, Nil()))))

    assertEquals(Cons("PPS", Cons("PCD", Nil())), getCoursesByList(listOfPersons))
    assertEquals(Nil(), getCoursesByList(Cons(studentOne,Cons(studentTwo, Nil()))))

  @Test def testFoldLeft(): Unit =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5,Nil()))))

    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    assertEquals(16, foldLeft(lst)(0)(_ + _))
    assertEquals(0, foldLeft(Nil[Int]())(0)(_ - _))

  @Test def testFoldRight(): Unit =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

    assertEquals(-8, foldRight(lst)(0)(_ - _))
    assertEquals(3, foldRight(Cons(3, Nil()))(0)(_ + _))
    assertEquals(3, foldRight(Cons(3, Nil()))(0)(_ - _))
    assertEquals(1, foldLeft(Nil[Int]())(1)(_ - _))
