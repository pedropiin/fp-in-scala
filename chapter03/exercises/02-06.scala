import java.lang.ArrayIndexOutOfBoundsException
import scala.annotation.tailrec

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

	def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
		as match {
			case Nil => z
			case Cons(x, xs) => f(x, foldRight(xs, z)(f))
		}
	}

	def sum_2(ns: List[Int]) = {
		foldRight(ns, 0)((x, y) => x + y)
	}
	
	def product_2(ns: List[Double]) = {
		foldRight(ns, 1.0)(_ * _)
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
	def drop_2[A](l: List[A], n: Int): List[A] = {
		n match {
			case x: Int if (x <= 0) => l
			case _ => drop_2(tail(l), n - 1)
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
	def init_2[A](l: List[A]): List[A] = {
		l match {
			case Nil => throw new ArrayIndexOutOfBoundsException
			case Cons(_, Nil) => Nil
			case Cons(h, t) => Cons(h, init(t))
		}
	}

	// Exercise 3.9
	def length[A](as: List[A]): Int = {
		foldRight(as, 0)((_, x) => x + 1)
	}

	// Exercise 3.10
	@annotation.tailrec
	def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
		l match {
			case Nil => z
			case Cons(h, t) => foldLeft(t, f(z, h))(f)
		}
	}

	// Exercise 3.11
	def sum3(l: List[Int]): Int = {
		foldLeft(l, 0)(_ + _)
	}
	def product3(l: List[Double]): Double = {
		foldLeft(l, 1.0)(_ * _)
	}
	def length_2[A](l: List[A]): Int = {
		foldLeft(l, 0)((z, _) => z + 1)
	}

	// Exercise 3.12
	def reverse[A](l: List[A]): List[A] = {
		foldLeft(l, Nil: List[A])((t, h) => Cons(h, t))
	}

	// Exercise 3.13
	def foldLeftViaRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
		foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
	}
	def foldRightViaLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
		foldLeft(reverse(l), z)((t, h) => f(h, t))
	}

	// Exercise 3.14
	def append_2[A](a1: List[A], a2: List[A]): List[A] = {
		foldRight(a1, a2)(Cons(_, _))
	}

	// Exercise 3.15
	def flatten[A](ll: List[List[A]]): List[A] = {
		foldLeft(ll, Nil)(append_2)
	}

	// Exercise 3.16
	def increment(l: List[Int], n: Int): List[Int] = {
		foldLeft(l, Nil)((t, h) => Cons(h + n, t))
	}

	// Exercise 3.17
	def doubleToString(l: List[Double]): List[String] = {
		foldLeft(l, Nil)((t, h) => Cons(h.toString(), t))
	}

	// Exercise 3.18
	def map[A, B](l: List[A])(f: A => B): List[B] = {
		foldLeft(l, Nil)((t, h) => Cons(f(h), t))
	}


	def main(args: Array[String]): Unit = {
		println(map(List(1, 2, 3, 4))((a: Int) => a.toString: String))
	}
}