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
      case Branch(left, right) => (depth(left) max depth(right)) + 1
      case null => throw new IllegalArgumentException
    }
  }

  // Exercise 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  // Exercise 3.29
  def fold[A, B](tree: Tree[A])(base: A => B)(f: (B, B) => B): B = {
    tree match {
      case Leaf(v) => base(v)
      case Branch(left, right) => f(fold(left)(base)(f), fold(right)(base)(f))
    }
  }
  def sizeViaFold[A](tree: Tree[A]): Int = {
    fold(tree)(x => 1)((x, y) => x + y)
  }
  def maxViaFold(tree: Tree[Int]): Int = {
    fold(tree)(x=> x)((x, y) => x max y)
  }
  def depthViaFold[A](tree: Tree[A]): Int = {
    fold(tree)(x => 1)((x, y) => (x max y) + 1)
  }
  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)((x: A) => Leaf(f(x)))((left: Tree[B], right: Tree[B]) => Branch(left, right))
  }

  def main(args: Array[String]): Unit = {
    val simpleTree: Tree[Int] = Branch(Branch(Leaf(4), Leaf(2)), Leaf(3))
    val complexTree: Tree[Int] = Branch(Leaf(5), Branch(Branch(Leaf(7), Branch(Leaf(5), Leaf(11))), Leaf(-2)))
    println(map(simpleTree)(x => 3 * x))
    println(mapViaFold(simpleTree)(x => 3 * x))
    println(map(complexTree)(x => 3 * x))
    println(mapViaFold(complexTree)(x => 3 * x))
  }
}
