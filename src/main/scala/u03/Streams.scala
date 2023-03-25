package u03

object Streams extends App:

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    import Lists.*

    def empty[A](): Stream[A] = Empty()

    def cons[A](head: => A, tail: => Stream[A]): Stream[A] =
      lazy val h = head
      lazy val t = tail
      Cons(() => h, () => t)

    def toList[A](stream: Stream[A]): List[A] = stream match
      case Cons(h,t) => List.Cons(h(),toList(t()))
      case _ => List.Nil()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    def map[A,B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(h,t) => cons(f(h()), map(t())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(f: A => Boolean): Stream[A] = stream match
      case Cons(h,t) if f(h()) => cons(h(), filter(t())(f))
      case Cons(_,t) => filter(t())(f)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream,n) match
      case (Cons(h,t),i) if i > 0 => cons(h(), take(t())(n-1))
      case _ => Empty()

    def drop[A](stream: Stream[A])(n: Int): Stream[A] = stream match
      case Cons(_,t) if n > 0 => drop(t())(n-1)
      case Cons(h,t) => cons(h(), drop(t())(0))
      case _ => Empty()

    def constant[A](const: A): Stream[A] =
      cons(const, constant(const))


    val fibs: Stream[Int] = map(cons((0,1), iterate((0,1))((x,y) => (y,x+y))))((x,y) => x+y)

    val corec: Stream[Int] = Stream.cons(1, corec)

  end Stream


