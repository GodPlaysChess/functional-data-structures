package data

import scala.annotation.tailrec
import data.basic.List

sealed abstract class Stream[A] {
  def drop(n: Int): Stream[A] = Stream.drop(n, this)
  def take(n: Int): Stream[A] = Stream.take(n, this)

  def concat(stream: Stream[A]): Stream[A] = Stream.concat(this, stream)
  def reverse: Stream[A] = Stream.reverse(this)

  def toList: List[A] = Stream.toList(this)
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
    case (0, _)          => stream
    case (_, End)        => stream
    case (_, Cons(h, t)) => Cons(h, () => take(n - 1, t()))
  }

  // todo tailrec
  def concat[A](stream1: Stream[A], stream2: Stream[A]): Stream[A] =
    stream1 match {
      case Stream.End => stream2
      case Cons(h, t) => Cons(h, () => concat(t(), stream2))
    }

  def reverse[A](stream: Stream[A]): Stream[A] = {
    @tailrec def go(rest: Stream[A], result: Stream[A]): Stream[A] =
      rest match {
        case Stream.End => result
        case Cons(h, t) => go(t(), Cons(h, () => result))
      }
    go(stream, end)
  }

  def toList[A](stream: Stream[A]): List[A] = {
    @tailrec def go(rest: Stream[A], result: List[A]): List[A] =
      rest match {
        case Stream.End => result
        case Cons(h, t) => go(t(), h() :: result)
      }
    go(stream, List.nil[A]).reverse
  }

  /*  Construction */
  def unfold[A, B](seed: A)(uf: A => Option[A]): Stream[A] = {
    uf(seed) match {
      case None => end
      case Some(x) => Cons(() => x, () => unfold(x)(uf))
    }
  }

}


