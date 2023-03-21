package u03
import scala.annotation.tailrec

/**
 * This is a simple implementation of a List.
 * Inside the List, we have two cases: Cons and Nil.
 * Cons is a case class that represents a non-empty list.
 * Nil is an object that represents an empty list.
 * operations:
 * - sum: sum all the elements of a list of integers
 * - append: append two lists
 * - flatMap: apply a function to each element of a list and then flatten the result
 * - map: apply a function to each element of a list
 * - filter: filter the elements of a list
 * - drop: drop the first n elements of a list
 * - max: find the maximum element of a list of integers
 * - getCoursesByList: get the courses of a list of persons
 * - foldLeft: fold a list from left to right
 * - foldRight: fold a list from right to left
 */
object Lists extends App :

  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  enum Option[A]:
    case Some(a: A)
    case None()

  // a companion object (i.e., module) for List
  object List:

    import Option.*
    import Person.*

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def append[A](l: List[A], r: List[A]): List[A] = (l, r) match
      case (Cons(h, t), r) => Cons(h, append(t, r))
      case (Nil(), r) => r

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case _ => Nil()

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(_,_) => flatMap(l)(a => Cons(mapper(a), Nil()))
      case Nil() => Nil()

    def filter[A](l: List[A])(pred: A => Boolean): List[A] = l match
      case Cons(_,_) => flatMap(l)(a => if pred(a) then Cons(a, Nil()) else Nil())
      case Nil() => Nil()

    @tailrec
    def drop[A](l: List[A], i: Int): List[A] = (l,i) match
      case (Cons(_, t),1) => t
      case (Cons(_, t),i) => drop(t,i-1)
      case (Nil(),_) => Nil()

    def max(l: List[Int]): Option[Int] = l match
      case Cons(h,t) => Some(max(t) match
        case Some(m) => if h > m then h else m
        case None() => h)
      case Nil() => None()

    def getCoursesByList(l: List[Person]): List[String] = flatMap(l)(p => p match
      case Student(_,_) => Nil()
      case Teacher(_,c) => Cons(c, Nil()))

    @tailrec
    def foldLeft[A,B](l: List[A])(acc: B)(op: (B,A) => B): B = l match
      case Cons(h,t) => foldLeft(t)(op(acc,h))(op)
      case Nil() => acc
    def foldRight[A,B](l: List[A])(acc: B)(op: (A,B) => B): B = l match
      case Cons(h,t) => op(h, foldRight(t)(acc)(op))
      case Nil() => acc
