package data.tree

sealed abstract class Tree[A: Ordering] {
  import Tree._
  private val O = implicitly[Ordering[A]]
  import O._

  def isEmpty: Boolean = cata(true, _ => false)

  def member(a: A): Boolean =
    cata(false, {
      case Node(left, el, right) =>
        if (a < el) left.member(a)
        else if (a > el) right.member(a)
        else true
    })

  def cata[B](caseEmpty: => B, caseNode: Node[A] => B): B = this match {
    case n @ Node(_, _, _) => caseNode(n)
    case Empty()           => caseEmpty
  }

}

object Tree {
  case class Node[A: Ordering](left: Tree[A], node: A, right: Tree[A]) extends Tree[A]
  case class Empty[A: Ordering]()                                      extends Tree[A]

}
