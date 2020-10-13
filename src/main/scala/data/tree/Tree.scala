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

  def insert(x: A): Tree[A] =
    cata(
      Tree.empty[A],
      node => {
        if (x < node.value) Node(node.left.insert(x), node.value, node.right)
        else if (x > node.value) Node(node.left, node.value, node.right.insert(x))
        else this
      }
    )

  def cata[B](caseEmpty: => B, caseNode: Node[A] => B): B = this match {
    case n @ Node(_, _, _) => caseNode(n)
    case Empty()           => caseEmpty
  }

}

object Tree {
  case class Node[A: Ordering](left: Tree[A], value: A, right: Tree[A]) extends Tree[A]
  case class Empty[A: Ordering]()                                       extends Tree[A]

  private val EmptyTree = Empty[Unit]()

  def empty[A: Ordering]: Tree[A] = EmptyTree.asInstanceOf[Tree[A]]

}
