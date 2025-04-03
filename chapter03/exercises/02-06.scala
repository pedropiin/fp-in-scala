import java.lang.ArrayIndexOutOfBoundsException

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
	def sum(ints: List[Int]): Int = {
		ints match {
			case Nil => 0
			case Cons(x, xs) => x + sum(xs)
		}
	}

	def product(ds: List[Double]): Double = {
		ds match {
			case Nil => 1.0
			case Cons(0.0, _) => 0.0
			case Cons(x, xs) => x * product(xs)
		}
	}

	def apply[A](as: A*): List[A] = {
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
	}

	def append[A](a1: List[A], a2: List[A]): List[A] = {
		a1 match {
			case Nil => a2
			case Cons(h, t) => Cons(h, append(t, a2))
		}
	}

	// Exercise 3.2
	def tail[A](l: List[A]): List[A] = {
		l match {
			case Nil => Nil 	// Good idea to throw exception
			case Cons(_: A, t) => t
		}
	}

	// Exercise 3.3
	def setHead[A](newElem: A, l: List[A]): List[A] = {
		l match {
			case Nil => List(newElem)
			case Cons(h, t) => Cons(newElem, t)
		}
	}

	// Exercise 3.4
	def drop[A](l: List[A], n: Int): List[A] = {
		@annotation.tailrec
		def loop(l: List[A], n: Int): List[A] = {
			n match {
				case 0 => l
				case _ => loop(tail(l), n - 1)
			}
		}
		loop(l, n)
	}
	def drop2[A](l: List[A], n: Int): List[A] = {
		n match {
			case x: Int if (x <= 0) => l
			case _ => drop2(tail(l), n - 1)
		}
	}

	// Exercise 3.5
	def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
		l match {
			case Nil => List()
			case Cons(h, t) => if (f(h)) dropWhile(t, f) else l 
		}
	}

	// Exercise 3.6
	def init[A](l: List[A]): List[A] = {
		def inner(og: List[A], ans: List[A]): List[A] = {
			og match {
				case Cons(h, Nil) => ans
				case Cons(h, t) => inner(t, append(ans, List(h)))
				case Nil => throw new ArrayIndexOutOfBoundsException
			}
		}
		inner(l, List())
	}
	def init2[A](l: List[A]): List[A] = {
		l match {
			case Nil => throw new ArrayIndexOutOfBoundsException
			case Cons(_, Nil) => Nil
			case Cons(h, t) => Cons(h, init(t))
		}
	}


	def main(args: Array[String]): Unit = {
		println(init(List(5, 4, 3, 2, 1)))
	}
}