package u03
import scala.annotation.tailrec

object Lists extends App :

  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  enum Person: // a sum type defined by enumerating various cases
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  enum Option[A]:
    case Some(a: A)
    case None() // here parens are needed because of genericity

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
