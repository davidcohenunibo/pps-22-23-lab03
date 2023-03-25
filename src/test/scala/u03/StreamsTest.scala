package u03

import org.junit.Test
import u03.Streams.Stream
import org.junit.Assert.*
import Lists.*
class StreamsTest {

  import List.*
  val str: Stream[Int] = Stream.iterate(0)(_ + 1)


  @Test
  def testEmpty(): Unit =
    val strEmpty = Stream.empty()
    assertEquals(Stream.empty(), strEmpty)

  @Test
  def testToList(): Unit =
    val f: Int => Int = x => x+1
    assertEquals(List.Cons(6, Nil()), Stream.toList(Stream.cons(f(5), Stream.empty())))
    assertEquals(Nil(), Stream.toList(Stream.empty()))

  @Test
  def testTake(): Unit =
    val f: Int => Int = x => x + 1
    assertEquals(List.Cons(6, Nil()), Stream.toList( Stream.take(Stream.cons(f(5), Stream.empty()))(3)))
    assertEquals(Nil(), Stream.toList(Stream.empty()))

  @Test
  def testIterate(): Unit =
    assertEquals(Cons(0,Cons(1,Cons(2,Cons(3,Cons(4,Nil()))))),
      Stream.toList(Stream.take(str)(5)))


  @Test
  def testMap(): Unit =
     assertEquals(Cons(10,Cons(11,Cons(12,Nil()))),
       Stream.toList(Stream.take(Stream.map(str)(_ + 10))(3))) // {1,2,3,4,..}


  @Test
  def testFilter(): Unit =
    assertEquals(Cons(0,Cons(1,Cons(2,Cons(21,Cons(22,Nil()))))),
      Stream.toList(Stream.take(Stream.filter(str)(x => x < 3 || x > 20))(5)))// {1,2,21,22,..}


  @Test
  def testDrop(): Unit =
    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    assertEquals(Cons(6,Cons(7,Cons(8,Cons(9,Nil())))), Stream.toList(Stream.drop(s)(6)))

  @Test
  def testConstant(): Unit =
    assertEquals(Cons("x",Cons("x",Cons("x",Cons("x",Cons("x",Nil()))))),Stream.toList(Stream.take(Stream.constant("x"))(5)))

  @Test
  def testFibonacci(): Unit =
    assertEquals(Cons(1,Cons(1,Cons(2,Cons(3,Nil())))),Stream.toList(Stream.take(Stream.fibs)(4)))

  @Test
  def testCorec(): Unit =
    assertEquals(Cons(1,Cons(1,Cons(1,Nil()))),Stream.toList(Stream.take(Stream.corec)(3)))
}
