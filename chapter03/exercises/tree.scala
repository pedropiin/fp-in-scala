import scala.annotation.tailrec
import java.lang.IllegalArgumentException

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // Exercise 3.25
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => size(left) + size(right) + 1
      case null => throw new IllegalArgumentException
    }
  }

  // Exercise 3.26
  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(v) => v
      case Branch(left, right) => maximum(left) max maximum(right)
      case null => throw new IllegalArgumentException
    }
  }

  // Exercise 3.27
  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => (depth(left) + 1) max (depth(right) + 1)
      case null => throw new IllegalArgumentException
    }
  }

  // Exercise 3.28
  def map[A, B](tree: Tree[A])(f: A => B): List[B] = {
    
  }

  def main(args: Array[String]): Unit = {
    val simpleTree: Tree[Int] = Branch(Branch(Leaf(4), Leaf(2)), Leaf(3))
    val complexTree: Tree[Int] = Branch(Leaf(5), Branch(Branch(Leaf(7), Branch(Leaf(5), Leaf(11))), Leaf(-2)))
    println(depth(complexTree))
  }
}
