package data

import scala.annotation.tailrec

sealed abstract class Stream[A] {
  def drop(n: Int): Stream[A] = Stream.drop(n, this)
  def take(n: Int): Stream[A] = Stream.take(n, this)

  def concat(stream: Stream[A]): Stream[A] = Stream.concat(this, stream)
  
}

object Stream {

  def end[A]: Stream[A] = End.asInstanceOf[Stream[A]]

  case object End extends Stream[A forSome { type A }]
  case class Cons[A](h: () => A, tail: () => Stream[A]) extends Stream[A] {}

  @tailrec
  def drop[A](n: Int, stream: Stream[A]): Stream[A] = (n, stream) match {
    case (0, _)          => stream
    case (_, End)        => stream
    case (_, Cons(_, t)) => drop[A](n - 1, t())
  }

  def take[A](n: Int, stream: Stream[A]): Stream[A] = (n, stream) match {
    case (0, _)          => result
    case (_, End)        => result
    case (_, Cons(h, t)) => Cons(h, () => take(n - 1, t()))
  }

  // todo tailrec
  def concat[A](stream1: Stream[A], stream2: Stream[A]): Stream[A] =
    stream1 match {
      case Stream.End => stream2
      case Cons(h, t) => Cons(h, () => concat(t(), stream2))
    }
}
